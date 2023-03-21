package composefree

import cats.Parallel
import io.estatico.newtype.macros.newtype
import scala.concurrent.{ExecutionContext, Future}

package object future {
  @newtype case class ParFuture[+A](run: Future[A])
  object ParFuture extends ParFutureInstances0

  implicit def parallelFutureParFuture(implicit ec: ExecutionContext): Parallel.Aux[Future, ParFuture] =
    composefree.future.ParFuture.parallelForParFuture
}
