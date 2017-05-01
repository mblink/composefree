package composefree

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scalaz.{Applicative, Coproduct, Free, FreeAp, Inject, Monad, ~>}
import scalaz.Inject._
import scalaz.Liskov.<~<

object puredsl {

  sealed trait PureOp[A]
  case class pure[A](a: A) extends PureOp[A]
}

trait LPSyntax {

  implicit class CommandOps[M[_], A](m: M[A]) {
    def as[G[_]](implicit ev: M[A] <~< G[A]): G[A] = ev(m)
  }
}

trait ComposeOps extends LPSyntax {

  class CNat[F[_], G[_], H[_]](f: F ~> H, g: G ~> H)
  extends (Coproduct[F, G, ?] ~> H) {
    type From[A] = Coproduct[F, G, A]
    def or[I[_]](i: I ~> H) = new CNat[From, I, H](this, i)
    def apply[A](fa: From[A]): H[A] = fa.run.fold(f(_), g(_))
    def |:[I[_]](i: I ~> H) = new CNat[I, From, H](i, this)
  }

  implicit class NTOps[F[_], G[_]](nt: F ~> G) {
    def or[H[_]](ont: H ~> G) = new CNat[F, H, G](nt, ont)
    def |:[H[_]](ont: H ~> G) = new CNat[H, F, G](ont, nt)
  }

  implicit class ComposeFreeApOps[G[_], A](p: FreeAp[G, A]) {
    def runAp[T[_]: Applicative](i: (G ~> T)): T[A] = p.foldMap(i)
    def as[GG[_]](implicit i: Inject[G, GG]): FreeAp[GG, A] = {
      type Out[B] = FreeAp[GG, B]
      p.foldMap(new (G ~> Out) { def apply[B](g: G[B]) = FreeAp.lift(i.inj(g)) })
    }
    def asM[GG[_]](implicit i: Inject[G, GG]): Free[GG, A] = {
      type Out[B] = Free[GG, B]
      p.foldMap(new (G ~> Out) { def apply[B](g: G[B]) = Free.liftF(i.inj(g)) })
    }
    def asM: Free[G, A] = {
      type Out[B] = Free[G, B]
      p.foldMap(new (G ~> Out) { def apply[B](g: G[B]) = Free.liftF(g) })
    }
  }

  implicit class ComposeFreeOps[M[_], A](p: Free[M, A]) {
    def runM[T[_]: Monad](i: (M ~> T)): T[A] = p.foldMap(i)
    def as[G[_]](implicit i: Inject[M, G]): Free[G, A] = {
      type Out[B] = Free[G, B]
      p.foldMap(new (M ~> Out) { def apply[B](m: M[B]) = Free.liftF(i.inj(m)) })
    }
  }

  object lift {
    implicit class ToFreeOps[M[_], A](m: M[A]) {
      def liftF: Free[M, A] = Free.liftF(m)
      def ap: FreeAp[M, A] = FreeAp.lift(m)
    }

    implicit def liftF[M[_], A](m: M[A]): Free[M, A] = m.liftF
  }
}

trait ComposeFree[M[_]] extends ComposeOps {

  type RecNode[A] = Coproduct[ComposeNode, M, A]
  type RecProg[A] = Free[RecNode, A]
  type RecApProg[A] = FreeAp[RecNode, A]

  type Composed[A] = RecProg[A]

  sealed trait ComposeNode[A]
  case class MNode[A](run: Free[RecNode, A]) extends ComposeNode[A]
  case class ANode[A](run: FreeAp[RecNode, A]) extends ComposeNode[A]


  def recInterp[G[_]](fg: (M ~> G))(implicit m: Monad[G], ap: Applicative[G])=
    new (ComposeNode ~> G) { self =>
      lazy val interp: (RecNode ~> G) =
        new (RecNode ~> G) {
          def apply[A](cp: RecNode[A]) =
            cp.fold(self, fg)
        }

      def apply[A](nf: ComposeNode[A]) = nf match {
        case MNode(mn) => mn.foldMap(interp)(m)
        case ANode(apn) => apn.foldMap(interp)(ap)
      }
    }

  implicit class FOps[F[_], A](fa: F[A])(implicit i: Inject[F, M]) {
    def op: RecProg[A] =
      Free.liftF(Coproduct.right[ComposeNode](i.inj(fa)): RecNode[A])
    def opAp: RecApProg[A] =
      FreeAp.lift(Coproduct.right[ComposeNode](i.inj(fa)): RecNode[A])
  }

  implicit class MOps[A](ma: M[A]) {
    def op: RecProg[A] =
      Free.liftF(Coproduct.right[ComposeNode](ma): RecNode[A])
    def opAp: RecApProg[A] =
      FreeAp.lift(Coproduct.right[ComposeNode](ma): RecNode[A])
  }

  implicit class ProgOps[A](pa: RecProg[A]) {
    def op: RecProg[A] = Free.liftF(Coproduct.left[M](MNode(pa)): RecNode[A])
    def opAp: RecApProg[A] = FreeAp.lift(Coproduct.left[M](MNode(pa)): RecNode[A])

    def runWith[G[_]](fg: (M ~> G))(implicit m: Monad[G], ap: Applicative[G]): G[A] =
      pa.foldMap(recInterp(fg)(m, ap).interp)
  }

  implicit class ApProgOps[A](apa: RecApProg[A]) {
    def op: RecProg[A] = Free.liftF(Coproduct.left[M](ANode(apa)): RecNode[A])
    def opAp: RecApProg[A] = FreeAp.lift(Coproduct.left[M](ANode(apa)): RecNode[A])
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[A]) {
    def op: RecProg[A] = Free.liftF(Coproduct.left[M](nfa): RecNode[A])
    def opAp: RecApProg[A] = FreeAp.lift(Coproduct.left[M](nfa): RecNode[A])

    def runWith[G[_]](fg: (M ~> G))(implicit m: Monad[G], ap: Applicative[G]): G[A] =
      nfa.op.foldMap(recInterp(fg)(m, ap).interp)
  }

  implicit class FreeApOps[A](p: FreeAp[M, A]) {
    def op: Composed[A] =
      p.foldMap(new (M ~> Composed) { def apply[B](m: M[B]) = m.op })
    def opAp: RecApProg[A] =
      p.foldMap(new (M ~> RecApProg) { def apply[B](m: M[B]) = m.opAp })
  }

  implicit class ComposeFreeOps[A](p: Free[M, A]) {
    def op: Composed[A] =
      p.foldMap(new (M ~> Composed) { def apply[B](m: M[B]) = m.op })
  }

  implicit def f2fnf[F[_], A](fa: F[A])(implicit i: Inject[F, M]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A]): Composed[A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[A]): Composed[A] = nfa.op

}

object syntax extends ComposeOps
