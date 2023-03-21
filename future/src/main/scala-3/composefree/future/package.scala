package composefree

import cats.Parallel
import scala.concurrent.{ExecutionContext, Future}

package object future {
  opaque type ParFuture[+A] = Future[A]
  object ParFuture extends ParFutureInstances0 {
    def apply[A](f: Future[A]): ParFuture[A] = f
    def deriving[T[_], A](implicit t: T[Future[A]]): T[ParFuture[A]] = t
  }

  extension[A](f: ParFuture[A]) def run: Future[A] = f

  given parallelFutureParFuture(using ec: ExecutionContext): Parallel.Aux[Future, ParFuture] =
    composefree.future.ParFuture.parallelForParFuture
}
