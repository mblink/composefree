package composefree

import cats.{~>, Applicative, Monad, Parallel}

sealed trait ComposeNode[M[_], A]
final case class MNode[M[_], A](run: RecProg[M, A]) extends ComposeNode[M, A]
final case class ANode[M[_], A](run: RecApProg[M, A]) extends ComposeNode[M, A]

object ComposeNode {
  implicit def recProgParallel[M[_]]: Parallel.Aux[RecProg[M, *], RecApProg[M, *]] =
    new Parallel[RecProg[M, *]] {
      type F[a] = RecApProg[M, a]
      val applicative = Applicative[RecApProg[M, *]]
      val monad = Monad[RecProg[M, *]]
      val parallel = new (RecProg[M, *] ~> RecApProg[M, *]) {
        def apply[A](p: RecProg[M, A]): RecApProg[M, A] = RecApProg.liftCN(MNode(p))
      }
      val sequential = new (RecApProg[M, *] ~> RecProg[M, *]) {
        def apply[A](p: RecApProg[M, A]): RecProg[M, A] = RecProg.liftCN(ANode(p))
      }
    }
}
