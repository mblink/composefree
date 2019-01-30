package composefree

import cats.{~>, Applicative, InjectK, Monad}
import cats.data.EitherK
import cats.evidence.As
import cats.free.{Free, FreeApplicative}
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls

object puredsl {
  sealed trait PureOp[A]
  case class pure[A](a: A) extends PureOp[A]
}

trait LPSyntax {
  implicit class CommandOps[M[_], A](m: M[A]) {
    def as[G[_]](implicit ev: As[M[A], G[A]]): G[A] = ev(m)
  }
}

trait ComposeOps extends LPSyntax {

  class CNat[F[_], G[_], H[_]](f: F ~> H, g: G ~> H)
  extends (EitherK[F, G, ?] ~> H) {
    type From[A] = EitherK[F, G, A]
    override def or[I[_]](i: I ~> H) = new CNat[From, I, H](this, i)
    def apply[A](fa: From[A]): H[A] = fa.run.fold(f(_), g(_))
    def |:[I[_]](i: I ~> H) = new CNat[I, From, H](i, this)
  }

  implicit class NTOps[F[_], G[_]](nt: F ~> G) {
    def or[H[_]](ont: H ~> G) = new CNat[F, H, G](nt, ont)
    def |:[H[_]](ont: H ~> G) = new CNat[H, F, G](ont, nt)
  }

  implicit class ComposeFreeApplicativeOps[G[_], A](p: FreeApplicative[G, A]) {
    def runAp[T[_]: Applicative](i: (G ~> T)): T[A] = p.foldMap(i)
    def as[GG[_]](implicit i: InjectK[G, GG]): FreeApplicative[GG, A] = {
      type Out[B] = FreeApplicative[GG, B]
      p.foldMap(new (G ~> Out) { def apply[B](g: G[B]) = FreeApplicative.lift(i.inj(g)) })
    }
    def asM[GG[_]](implicit i: InjectK[G, GG]): Free[GG, A] = {
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
    def as[G[_]](implicit i: InjectK[M, G]): Free[G, A] = {
      type Out[B] = Free[G, B]
      p.foldMap(new (M ~> Out) { def apply[B](m: M[B]) = Free.liftF(i.inj(m)) })
    }
  }

  object lift {
    implicit class ToFreeOps[M[_], A](m: M[A]) {
      def liftF: Free[M, A] = Free.liftF(m)
      def ap: FreeApplicative[M, A] = FreeApplicative.lift(m)
    }

    implicit def liftF[M[_], A](m: M[A]): Free[M, A] = m.liftF
  }
}

trait ComposeFree[M[_]] extends ComposeOps {

  type RecNode[A] = EitherK[ComposeNode, M, A]
  type RecProg[A] = Free[RecNode, A]
  type RecApProg[A] = FreeApplicative[RecNode, A]

  type Composed[A] = RecProg[A]

  sealed trait ComposeNode[A]
  case class MNode[A](run: Free[RecNode, A]) extends ComposeNode[A]
  case class ANode[A](run: FreeApplicative[RecNode, A]) extends ComposeNode[A]


  def recInterp[G[_]](fg: (M ~> G))(implicit M: Monad[G]) =
    new (ComposeNode ~> G) { self =>
      lazy val interp: (RecNode ~> G) =
        new (RecNode ~> G) {
          def apply[A](cp: RecNode[A]) =
            cp.fold(self, fg)
        }

      def apply[A](nf: ComposeNode[A]) = nf match {
        case MNode(mn) => mn.foldMap(interp)
        case ANode(apn) => apn.foldMap(interp)(M)
      }
    }

  implicit class FOps[F[_], A](fa: F[A])(implicit i: InjectK[F, M]) {
    def op: RecProg[A] =
      Free.liftF(EitherK.right[ComposeNode](i.inj(fa)): RecNode[A])
    def opAp: RecApProg[A] =
      FreeApplicative.lift(EitherK.right[ComposeNode](i.inj(fa)): RecNode[A])
  }

  implicit class MOps[A](ma: M[A]) {
    def op: RecProg[A] =
      Free.liftF(EitherK.right[ComposeNode](ma): RecNode[A])
    def opAp: RecApProg[A] =
      FreeApplicative.lift(EitherK.right[ComposeNode](ma): RecNode[A])
  }

  implicit class ProgOps[A](pa: RecProg[A]) {
    def op: RecProg[A] = Free.liftF(EitherK.left[M](MNode(pa)): RecNode[A])
    def opAp: RecApProg[A] = FreeApplicative.lift(EitherK.left[M](MNode(pa)): RecNode[A])

    def runWith[G[_]: Monad](fg: (M ~> G)): G[A] =
      pa.foldMap(recInterp(fg).interp)
  }

  implicit class ApProgOps[A](apa: RecApProg[A]) {
    def op: RecProg[A] = Free.liftF(EitherK.left[M](ANode(apa)): RecNode[A])
    def opAp: RecApProg[A] = FreeApplicative.lift(EitherK.left[M](ANode(apa)): RecNode[A])
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[A]) {
    def op: RecProg[A] = Free.liftF(EitherK.left[M](nfa): RecNode[A])
    def opAp: RecApProg[A] = FreeApplicative.lift(EitherK.left[M](nfa): RecNode[A])

    def runWith[G[_]: Monad](fg: (M ~> G)): G[A] =
      nfa.op.foldMap(recInterp(fg).interp)
  }

  implicit class FreeApplicativeOps[A](p: FreeApplicative[M, A]) {
    def op: Composed[A] =
      p.foldMap(new (M ~> Composed) { def apply[B](m: M[B]) = m.op })
    def opAp: RecApProg[A] =
      p.foldMap(new (M ~> RecApProg) { def apply[B](m: M[B]) = m.opAp })
  }

  implicit class ComposeFreeOps[A](p: Free[M, A]) {
    def op: Composed[A] =
      p.foldMap(new (M ~> Composed) { def apply[B](m: M[B]) = m.op })
  }

  implicit def f2fnf[F[_], A](fa: F[A])(implicit i: InjectK[F, M]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A]): Composed[A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[A]): Composed[A] = nfa.op
}

object syntax extends ComposeOps
