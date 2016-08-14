package composefree

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Coproduct, Free, Inject, Monad, ~>}
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

  implicit class ComposeFreeOps[M[_], A](p: Free[M, A]) {
    def runWith[T[_]: Monad](i: (M ~> T)): T[A] = p.foldMap(i)
    def as[G[_]](implicit i: Inject[M, G]): Free[G, A] = {
      type Out[B] = Free[G, B]
      p.foldMap(new (M ~> Out) { def apply[B](m: M[B]) = Free.liftF(i.inj(m)) })
    }
  }

  object lift {
    implicit class ToFreeOps[M[_], A](m: M[A]) {
      def liftF: Free[M, A] = Free.liftF(m)
    }

    implicit def liftF[M[_], A](m: M[A]): Free[M, A] = m.liftF
  }
}

trait ComposeFree[M[_]] extends ComposeOps {

  implicit class MkOp[F[_], A](fa: F[A]) {
    def op(implicit i: Inject[F, M]): Free[M, A] = Free.liftF(i.inj(fa))
  }

  implicit def mkOp[F[_], A](fa: F[A])(implicit i: Inject[F, M]): Free[M, A] =
    fa.op
}

object syntax extends ComposeOps
