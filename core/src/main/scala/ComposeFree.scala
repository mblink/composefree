package composefree

import scala.language.higherKinds
import scala.language.implicitConversions
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
  }

  implicit class NTOps[F[_], G[_]](nt: F ~> G) {
    def or[H[_]](ont: H ~> G) = new CNat[F, H, G](nt, ont)
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

  type ApM[G[_], X] = Coproduct[FreeAp[Free[G, ?], ?], Free[G, ?], X]

  implicit class ComposeFreeApMOps[G[_], A](p: Free[ApM[G, ?], A]) {
    def runWith[T[_]: Applicative: Monad](a: (G ~> T), m: (G ~> T)): T[A] =
      p.foldMap(new (ApM[G, ?] ~> T) { def apply[X](x: ApM[G, X]) =
        x.run.fold(_.foldMap(new (Free[G, ?] ~> T) {
          def apply[Z](x: Free[G, Z]) = x.foldMap(a) }),
          _.foldMap(m)) })

    def runWith[T[_]: Applicative: Monad](i: (G ~> T)): T[A] = runWith(i, i)
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

  type FM[A] = Free[M, A]
  type FA[A] = FreeAp[FM, A]
  type Composed[A] = Free[Coproduct[FA, FM, ?], A]

  implicit class MkOp[F[_], A](fa: F[A]) {
    def op(implicit i: Inject[F, M]): FM[A] = Free.liftF(i.inj(fa))
    def opAp(implicit i: Inject[F, M]): FA[A] = FreeAp.lift(Free.liftF(i.inj(fa)): FM[A])
    def apM(implicit i: Inject[F, M]): Composed[A] = fa.op.op
  }

  implicit class MKFOp[A](fa: FM[A]) {
    def op: Composed[A] = Free.liftF[Coproduct[FA, FM, ?], A](Coproduct.right[FA](fa))

    def opAp: FA[A] = FreeAp.lift(fa)

    def runWith[T[_]: Applicative: Monad](a: (M ~> T), m: (M ~>T)): T[A] =
      fa.op.runWith(a, m)
    def runWith[T[_]: Applicative: Monad](i: (M ~> T)): T[A] = runWith(i, i)
  }

  implicit class MKFAOp[A](fa: FA[A]) {
    def op: Composed[A] = Free.liftF[Coproduct[FA, FM, ?], A](Coproduct.left[FM](fa))

    def runWith[T[_]: Applicative: Monad](a: (M ~> T), m: (M ~>T)): T[A] =
      fa.op.runWith(a, m)
    def runWith[T[_]: Applicative: Monad](i: (M ~> T)): T[A] = runWith(i, i)
  }

  implicit def mkOpM[F[_], A](fa: F[A])(implicit i: Inject[F, M]): Free[M, A] = fa.op
  implicit def mkFOp[A](fa: Free[M, A]): Composed[A] = fa.op
  implicit def mkFAOp[A](fa: FreeAp[Free[M, ?], A]): Composed[A] = fa.op

}

object syntax extends ComposeOps
