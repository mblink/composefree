package composefree

import cats.~>
import cats.free.{Free, FreeApplicative}

sealed trait ComposeNode[M[_], A]
final case class MNode[M[_], A](run: RecProg[M, A]) extends ComposeNode[M, A]
final case class ANode[M[_], A](run: FreeApplicative[RecNode[M, *], A]) extends ComposeNode[M, A]

type RecNode[M[_], A] = ComposeNode[M, A] | M[A]
object RecNode {
  inline def fold[M[_], F[_], A](n: RecNode[M, A])(c: ComposeNode[M, *] ~> F, m: M ~> F): F[A] =
    n match {
      case x: ComposeNode[M, A] @unchecked => c(x)
      case x: M[A] @unchecked => m(x)
    }

  inline def liftM[M[_], A](m: M[A]): RecNode[M, A] = m
  inline def liftCN[M[_], A](c: ComposeNode[M, A]): RecNode[M, A] = c
}

type RecProg[M[_], A] = Free[RecNode[M, *], A]
object RecProg {
  inline def liftM[M[_], A](m: M[A]): RecProg[M, A] = Free.liftF(m)
  inline def liftCN[M[_], A](c: ComposeNode[M, A]): RecProg[M, A] = Free.liftF(c)
}

type RecApProg[M[_], A] = FreeApplicative[RecNode[M, *], A]
object RecApProg {
  inline def liftM[M[_], A](m: M[A]): RecApProg[M, A] = FreeApplicative.lift(m)
  inline def liftCN[M[_], A](c: ComposeNode[M, A]): RecApProg[M, A] = FreeApplicative.lift(c)
}

type Composed[M[_], A] = RecProg[M, A]

trait PackageCompat
