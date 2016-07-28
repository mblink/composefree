package composefree

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Coproduct, Free, Inject, Monad, ~>}
import scalaz.Inject._

object puredsl {

  sealed trait PureOp[A]
  case class Pure[A](a: A) extends PureOp[A]
  def pure[A](a: A): PureOp[A] = Pure(a)
}

trait ComposeFree[M[_]] {

  implicit class MkOp[F[_], A](fa: F[A]) {
    def op(implicit i: Inject[F, M]): Free[M, A] = Free.liftF(i.inj(fa))
  }

  implicit def mkOp[F[_], A](fa: F[A])(implicit i: Inject[F, M]): Free[M, A] =
    fa.op

  class CNat[F[_], G[_], H[_]](f: F ~> H, g: G ~> H)
  extends (Coproduct[F, G, ?] ~> H) {
    type From[A] = Coproduct[F, G, A]
    def or[I[_]](i: I ~> H) = new CNat[From, I, H](this, i)
    def apply[A](fa: From[A]): H[A] = fa.run.fold(f(_), g(_))
  }

  implicit class NTOps[F[_], G[_]](nt: F ~> G) {
    def or[H[_]](ont: H ~> G) = new CNat[F, H, G](nt, ont)
  }

  implicit class ComposeFreeOps[A](p: Free[M, A]) {
    def runWith[T[_]: Monad](i: (M ~> T)): T[A] = p.foldMap(i)
  }
}
