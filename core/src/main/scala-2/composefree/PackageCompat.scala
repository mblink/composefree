package composefree

import cats.~>
import cats.data.EitherK
import cats.free.{Free, FreeApplicative}

trait PackageCompat {
  final type RecNode[M[_], A] = EitherK[ComposeNode[M, *], M, A]
  object RecNode {
    @inline def fold[M[_], F[_], A](n: RecNode[M, A])(c: ComposeNode[M, *] ~> F, m: M ~> F): F[A] = n.fold(c, m)

    @inline def liftM[M[_], A](m: M[A]): RecNode[M, A] = EitherK.right(m)
    @inline def liftCN[M[_], A](c: ComposeNode[M, A]): RecNode[M, A] = EitherK.left(c)
  }

  final type RecProg[M[_], A] = Free[RecNode[M, *], A]
  object RecProg {
    @inline def liftM[M[_], A](m: M[A]): RecProg[M, A] = Free.liftF(EitherK.right(m))
    @inline def liftCN[M[_], A](c: ComposeNode[M, A]): RecProg[M, A] = Free.liftF(EitherK.left(c))
  }

  final type RecApProg[M[_], A] = FreeApplicative[RecNode[M, *], A]
  object RecApProg {
    @inline def liftM[M[_], A](m: M[A]): RecApProg[M, A] = FreeApplicative.lift(EitherK.right(m))
    @inline def liftCN[M[_], A](c: ComposeNode[M, A]): RecApProg[M, A] = FreeApplicative.lift(EitherK.left(c))
  }

  final type Composed[M[_], A] = RecProg[M, A]
}
