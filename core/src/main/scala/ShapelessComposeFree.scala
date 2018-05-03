package composefree

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scalaz.{~>, Applicative, Free, FreeAp, Monad}
import scalaz.syntax.either._
import shapeless.{:+:, CNil, Coproduct}
import shapeless.ops.coproduct.{Basis, Inject}

trait ShapelessComposeOps extends LPSyntax {
  class CNat[F[_], G[_] <: Coproduct, H[_]](f: F ~> H, g: G ~> H) extends (Lambda[a => F[a] :+: G[a]] ~> H) {
    type From[A] = F[A] :+: G[A]
    def apply[A](fa: From[A]): H[A] = fa.eliminate(f(_), g(_))
    def |:[I[_]](i: I ~> H) = new CNat[I, From, H](i, this)
  }

  type CNilF[A] = CNil
  object CNilF {
    def interp[F[_]] = new (CNilF ~> F) { def apply[A](c: CNilF[A]) = c.impossible }
    def |:[F[_], H[_]](nt: F ~> H) = new CNat[F, CNilF, H](nt, interp)
  }

  implicit class NTOps[F[_], H[_]](nt: F ~> H) {
    def |:[G[_]](ont: G ~> H) = ont |: nt |: CNilF
  }
}

trait ComposeFreeNoRecursion[M[_] <: Coproduct] extends ShapelessComposeOps {
  type RecNode[A] = M[A]
  type RecProg[A] = Free[RecNode, A]
  type RecApProg[A] = FreeAp[RecNode, A]

  type Composed[A] = RecProg[A]

  implicit class FOps[F[_], A](fa: F[A])(implicit inj: Inject[RecNode[A], F[A]]) {
    def op: RecProg[A] = Free.liftF(inj(fa))
    def opAp: RecApProg[A] = FreeAp.lift(inj(fa))
  }

  implicit class MOps[A](ma: M[A]) {
    def op: RecProg[A] = Free.liftF(ma)
    def opAp: RecApProg[A] = FreeAp.lift(ma)
  }

  implicit def f2fnf[F[_], A](fa: F[A])(implicit inj: Inject[RecNode[A], F[A]]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A]): Composed[A] = ma.op
}

trait ShapelessComposeFree[M[_] <: Coproduct] extends ShapelessComposeOps {
  type RecNode[A] = ComposeNode[A] :+: M[A]
  type RecProg[A] = Free[RecNode, A]
  type RecApProg[A] = FreeAp[RecNode, A]

  type Composed[A] = RecProg[A]

  sealed trait ComposeNode[A]
  case class MNode[A](run: Free[RecNode, A]) extends ComposeNode[A]
  case class ANode[A](run: FreeAp[RecNode, A]) extends ComposeNode[A]

  def recInterp[G[_]](fg: (M ~> G))(implicit m: Monad[G], ap: Applicative[G]) = new (ComposeNode ~> G) { self =>
    lazy val interp: (RecNode ~> G) = new (RecNode ~> G) {
      def apply[A](cp: RecNode[A]) = cp.eliminate(self(_), fg(_))
    }

    def apply[A](nf: ComposeNode[A]) = nf match {
      case MNode(mn) => mn.foldMap(interp)(m)
      case ANode(apn) => apn.foldMap(interp)(ap)
    }
  }

  implicit class FOps[F[_], A](fa: F[A])(implicit inj: Inject[RecNode[A], F[A]]) {
    def op: RecProg[A] = Free.liftF(inj(fa))
    def opAp: RecApProg[A] = FreeAp.lift(inj(fa))
  }

  implicit class MOps[A](ma: M[A])(implicit mInj: Inject[RecNode[A], M[A]]) {
    def op: RecProg[A] = Free.liftF(mInj(ma))
    def opAp: RecApProg[A] = FreeAp.lift(mInj(ma))
  }

  implicit class ProgOps[A](pa: RecProg[A]) {
    def op: RecProg[A] = Free.liftF(Coproduct[RecNode[A]](MNode(pa): ComposeNode[A]))
    def opAp: RecApProg[A] = FreeAp.lift(Coproduct[RecNode[A]](MNode(pa): ComposeNode[A]))

    def runWith[G[_]](fg: (M ~> G))(implicit m: Monad[G], ap: Applicative[G]): G[A] =
      pa.foldMap(recInterp(fg)(m, ap).interp)
  }

  implicit class ApProgOps[A](apa: RecApProg[A]) {
    def op: RecProg[A] = Free.liftF(Coproduct[RecNode[A]](ANode(apa): ComposeNode[A]))
    def opAp: RecApProg[A] = FreeAp.lift(Coproduct[RecNode[A]](ANode(apa): ComposeNode[A]))
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[A]) {
    def op: RecProg[A] = Free.liftF(Coproduct[RecNode[A]](nfa))
    def opAp: RecApProg[A] = FreeAp.lift(Coproduct[RecNode[A]](nfa))

    def runWith[G[_]](fg: (M ~> G))(implicit m: Monad[G], ap: Applicative[G]): G[A] =
      nfa.op.foldMap(recInterp(fg)(m, ap).interp)
  }

  implicit def f2fnf[F[_], A](fa: F[A])(implicit inj: Inject[RecNode[A], F[A]]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A])(implicit mInj: Inject[RecNode[A], M[A]]): Composed[A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[A]): Composed[A] = nfa.op

  implicit def subCoproductFree[Sub[_] <: Coproduct, A](sub: Free[Sub, A])(implicit b: Basis[RecNode[A], Sub[A]], m: Monad[Sub]): Composed[A] =
    Free.liftF(b.inverse(sub.extractF.right[b.type#Rest].toEither))

  // I think this is the path that this needs to go, but can't figure out the Sub ~> RecNode
  // transformation as noted below
  // implicit def subCoproductFree2[Sub[_] <: Coproduct, A](sub: Free[Sub, A])(implicit b: Basis[RecNode[A], Sub[A]]): Composed[A] =
  //   sub.mapSuspension(/* need Sub ~> RecNode using Basis */)
}

object shapelessSyntax extends ShapelessComposeOps
