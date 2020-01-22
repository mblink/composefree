package composefree

import cats.{~>, Monad}
import cats.free.{Free, FreeApplicative}
import cats.evidence.As
import composefree.{Composed => C, ComposedAp => CA}
import freek._

trait ComposeOps {
  implicit class MAsOps[M[_], A](m: M[A]) {
    def as[G[_]](implicit ev: As[M[A], G[A]]): G[A] = ev(m)
  }

  object lift {
    implicit class ToFreeOps[M[_], A](m: M[A]) {
      def liftF: Free[M, A] = Free.liftF(m)
      def ap: FreeApplicative[M, A] = FreeApplicative.lift(m)
    }

    implicit def liftF[M[_], A](m: M[A]): Free[M, A] = m.liftF
  }
}

sealed abstract class ComposedInterp[F[_] <: CopK[_], G[_]] extends (ComposeNode[RecNode[F, ?], ?] ~> G) {
  def interp: (RecNode[F, ?] ~> G)
}

object ComposedInterp {
  def apply[F[_] <: CopK[_], G[_]: Monad](fg: F ~> G): ComposedInterp[F, G] = new ComposedInterp[F, G] { self =>
    lazy val interp = Lambda[RecNode[F, ?] ~> G](_.run.fold(self(_), fg(_)))
    def apply[A](cn: ComposeNode[RecNode[F, ?], A]) = cn.fold(_.foldMap(interp), _.foldMap(interp))
  }
}

trait ComposedSyntax {
  implicit class ComposedRunOps[F[_] <: CopK[_], A](fa: Composed[F, A]) {
    def runWith[G[_]: Monad](fg: (F ~> G)): G[A] =
      fa.foldMap(ComposedInterp(fg).interp)

    def runWith[M[_] <: CopK[_], G[_]: Monad](i: Interpreter[M, G])(implicit s: SubCop[F, M]): G[A] =
      runWith(Lambda[F ~> G](m => i.nat(s(m))))
  }
}

sealed abstract class ComposeFreeLP0[D <: DSL, M0[_] <: CopK[_]](val M: DSL.Make[D, M0])
extends ComposeOps {
  type M[a] = M0[a]

  type Composed[A] = C[M, A]
  type ComposedAp[A] = CA[M, A]

  private lazy val mToRecNode = Lambda[M ~> RecNode[M, ?]](RecNode(_))
  private lazy val mToComposed = Lambda[M ~> Composed](m => Free.liftF(mToRecNode(m)))
  private lazy val mToComposedAp = Lambda[M ~> ComposedAp](m => FreeApplicative.lift(mToRecNode(m)))

  private def fToRecNode[F[_]](implicit s: SubCop[In1[F, ?], M]): F ~> RecNode[M, ?] =
    Lambda[F ~> RecNode[M, ?]](x => mToRecNode(s(In1(x))))

  private def freeToFreeAp[F[_] <: CopK[_]]: C[F, ?] ~> CA[F, ?] =
    Lambda[C[F, ?] ~> CA[F, ?]](f => FreeApplicative.lift(RecNode(f)))

  private def freeApToFree[F[_] <: CopK[_]]: CA[F, ?] ~> C[F, ?] =
    Lambda[CA[F, ?] ~> C[F, ?]](f => Free.liftF(RecNode(f)))

  private def otherCNToThis[F[_] <: CopK[_]](implicit s: SubCop[F, M]): ComposeNode[RecNode[F, ?], ?] ~> ComposeNode[RecNode[M, ?], ?] =
    Lambda[ComposeNode[RecNode[F, ?], ?] ~> ComposeNode[RecNode[M, ?], ?]](_.fold(
      m => ComposeNode(m.mapK(otherRNToThis[F])),
      a => ComposeNode(a.compile(otherRNToThis[F]))))

  private def otherRNToThis[F[_] <: CopK[_]](implicit s: SubCop[F, M]): RecNode[F, ?] ~> RecNode[M, ?] =
    Lambda[RecNode[F, ?] ~> RecNode[M, ?]](_.run.fold(
      cn => RecNode(otherCNToThis.apply(cn)),
      fa => RecNode(s(fa))))

  implicit class MOps[A](ma: M[A]) {
    def op: Composed[A] = mToComposed(ma)
    def opAp: ComposedAp[A] = mToComposedAp(ma)
  }

  implicit class FOps[F[_], A](fa: F[A])(implicit s: SubCop[In1[F, ?], M]) {
    def op: Composed[A] = Free.liftF(mToRecNode(s(In1(fa))))
    def opAp: ComposedAp[A] = FreeApplicative.lift(mToRecNode(s(In1(fa))))
  }

  implicit class FreeFOps[F[_], A](fa: Free[F, A])(implicit s: SubCop[In1[F, ?], M]) {
    def op: Composed[A] = fa.mapK(fToRecNode[F])
    def opAp: ComposedAp[A] = freeToFreeAp(fa.mapK(fToRecNode[F]))
  }

  implicit class FreeMOps[A](fa: Free[M, A]) {
    def op: Composed[A] = fa.mapK(mToRecNode)
    def opAp: ComposedAp[A] = freeToFreeAp(fa.mapK(mToRecNode))
  }

  implicit class FreeApplicativeFOps[F[_], A](fa: FreeApplicative[F, A])(implicit s: SubCop[In1[F, ?], M]) {
    def op: Composed[A] = freeApToFree(fa.compile(fToRecNode[F]))
    def opAp: ComposedAp[A] = fa.compile(fToRecNode[F])
  }

  implicit class FreeApplicativeMOps[A](p: FreeApplicative[M, A]) {
    def op: Composed[A] = p.foldMap(mToComposed)
    def opAp: ComposedAp[A] = p.foldMap(mToComposedAp)
  }

  implicit class ComposedOps[F[_] <: CopK[_], A](fa: C[F, A])(implicit s: SubCop[F, M]) {
    def op: Composed[A] = fa.mapK(otherRNToThis)
    def opAp: ComposedAp[A] = freeToFreeAp(fa.mapK(otherRNToThis))
  }

  implicit class ComposedApOps[F[_] <: CopK[_], A](fa: CA[F, A])(implicit s: SubCop[F, M]) {
    def op: Composed[A] = freeApToFree(fa.compile(otherRNToThis))
    def opAp: ComposedAp[A] = fa.compile(otherRNToThis)
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[RecNode[M, ?], A]) {
    def op: Composed[A] = Free.liftF(RecNode(nfa))
    def opAp: ComposedAp[A] = FreeApplicative.lift(RecNode(nfa))
  }
}

abstract class ComposeFree[D <: DSL, M[_] <: CopK[_]](M: DSL.Make[D, M])
extends ComposeFreeLP0[D, M](M) { self =>
  implicit def liftFA[F[_], A](fa: F[A])(implicit s: SubCop[In1[F, ?], M]): Composed[A] = fa.op
  implicit def liftMA[A](ma: M[A]): Composed[A] = ma.op
  implicit def liftComposeNode[A](cn: ComposeNode[RecNode[M, ?], A]): Composed[A] = cn.op

  def |>[A](f: ComposeFree[D, M] => A): A = f(self)
}

object ComposeFree {
  def apply[D <: DSL](implicit toCop: ToCopK[D]): ComposeFree[D, toCop.Cop] =
    new ComposeFree[D, toCop.Cop](DSL.Make[D]) {}
}

object syntax extends ComposeOps
