package composefree

import cats.{~>, Monad}
import cats.free.{Free, FreeApplicative}
import cats.evidence.As
import freek._

object puredsl {
  sealed trait PureOp[A]
  case class pure[A](a: A) extends PureOp[A]
}

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

sealed abstract class RecInterp[F[_] <: CopK[_], G[_]] extends (ComposeNode[RecNode[F, ?], ?] ~> G) {
  def interp: (RecNode[F, ?] ~> G)
}

object RecInterp {
  def apply[F[_] <: CopK[_], G[_]: Monad](fg: F ~> G): RecInterp[F, G] = new RecInterp[F, G] { self =>
    lazy val interp = Lambda[RecNode[F, ?] ~> G](_.run.fold(self(_), fg(_)))
    def apply[A](cn: ComposeNode[RecNode[F, ?], A]) = cn.fold(_.foldMap(interp), _.foldMap(interp))
  }
}

trait RecProgSyntax {
  implicit class RecProgRunOps[F[_] <: CopK[_], A](fa: RecProg[F, A]) {
    def runWith[G[_]: Monad](fg: (F ~> G)): G[A] =
      fa.foldMap(RecInterp(fg).interp)

    def runWith[M[_] <: CopK[_], G[_]: Monad](i: Interpreter[M, G])(implicit s: SubCop[F, M]): G[A] =
      runWith(Lambda[F ~> G](m => i.nat(s(m))))
  }
}

sealed abstract class ComposeFreeLP0[D <: DSL, M0[_] <: CopK[_]](val M: DSL.Make[D, M0])
extends ComposeOps {
  type M[a] = M0[a]

  type RP[F[_] <: CopK[_], A] = Free[RecNode[F, ?], A]
  type RAP[F[_] <: CopK[_], A] = FreeApplicative[RecNode[F, ?], A]

  type RecProg[A] = RP[M, A]
  type RecApProg[A] = RAP[M, A]
  type Composed[A] = RecProg[A]

  private lazy val mToRecNode = Lambda[M ~> RecNode[M, ?]](RecNode(_))
  private lazy val mToRecProg = Lambda[M ~> RecProg](m => Free.liftF(mToRecNode(m)))
  private lazy val mToRecApProg = Lambda[M ~> RecApProg](m => FreeApplicative.lift(mToRecNode(m)))

  private def fToRecNode[F[_]](implicit s: SubCop[In1[F, ?], M]): F ~> RecNode[M, ?] =
    Lambda[F ~> RecNode[M, ?]](x => mToRecNode(s(In1(x))))

  private def freeToFreeAp[F[_] <: CopK[_]]: RP[F, ?] ~> RAP[F, ?] =
    Lambda[RP[F, ?] ~> RAP[F, ?]](f => FreeApplicative.lift(RecNode(f)))

  private def freeApToFree[F[_] <: CopK[_]]: RAP[F, ?] ~> RP[F, ?] =
    Lambda[RAP[F, ?] ~> RP[F, ?]](f => Free.liftF(RecNode(f)))

  private def otherCNToThis[F[_] <: CopK[_]](implicit s: SubCop[F, M]): ComposeNode[RecNode[F, ?], ?] ~> ComposeNode[RecNode[M, ?], ?] =
    Lambda[ComposeNode[RecNode[F, ?], ?] ~> ComposeNode[RecNode[M, ?], ?]](_.fold(
      m => ComposeNode(m.mapK(otherRNToThis[F])),
      a => ComposeNode(a.compile(otherRNToThis[F]))))

  private def otherRNToThis[F[_] <: CopK[_]](implicit s: SubCop[F, M]): RecNode[F, ?] ~> RecNode[M, ?] =
    Lambda[RecNode[F, ?] ~> RecNode[M, ?]](_.run.fold(
      cn => RecNode(otherCNToThis.apply(cn)),
      fa => RecNode(s(fa))))

  implicit class MOps[A](ma: M[A]) {
    def op: RecProg[A] = mToRecProg(ma)
    def opAp: RecApProg[A] = mToRecApProg(ma)
  }

  implicit class FOps[F[_], A](fa: F[A])(implicit s: SubCop[In1[F, ?], M]) {
    def op: RecProg[A] = Free.liftF(mToRecNode(s(In1(fa))))
    def opAp: RecApProg[A] = FreeApplicative.lift(mToRecNode(s(In1(fa))))
  }

  implicit class FreeFOps[F[_], A](fa: Free[F, A])(implicit s: SubCop[In1[F, ?], M]) {
    def op: RecProg[A] = fa.mapK(fToRecNode[F])
    def opAp: RecApProg[A] = freeToFreeAp(fa.mapK(fToRecNode[F]))
  }

  implicit class FreeMOps[A](fa: Free[M, A]) {
    def op: RecProg[A] = fa.mapK(mToRecNode)
    def opAp: RecApProg[A] = freeToFreeAp(fa.mapK(mToRecNode))
  }

  implicit class FreeApplicativeFOps[F[_], A](fa: FreeApplicative[F, A])(implicit s: SubCop[In1[F, ?], M]) {
    def op: RecProg[A] = freeApToFree(fa.compile(fToRecNode[F]))
    def opAp: RecApProg[A] = fa.compile(fToRecNode[F])
  }

  implicit class FreeApplicativeMOps[A](p: FreeApplicative[M, A]) {
    def op: RecProg[A] = p.foldMap(mToRecProg)
    def opAp: RecApProg[A] = p.foldMap(mToRecApProg)
  }

  implicit class RecProgOps[F[_] <: CopK[_], A](fa: RP[F, A])(implicit s: SubCop[F, M]) {
    def op: RecProg[A] = fa.mapK(otherRNToThis)
    def opAp: RecApProg[A] = freeToFreeAp(fa.mapK(otherRNToThis))
  }

  implicit class RecApProgOps[F[_] <: CopK[_], A](fa: RAP[F, A])(implicit s: SubCop[F, M]) {
    def op: RecProg[A] = freeApToFree(fa.compile(otherRNToThis))
    def opAp: RecApProg[A] = fa.compile(otherRNToThis)
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[RecNode[M, ?], A]) {
    def op: RecProg[A] = Free.liftF(RecNode(nfa))
    def opAp: RecApProg[A] = FreeApplicative.lift(RecNode(nfa))

    def runWith[G[_]: Monad](fg: (M ~> G)): G[A] =
      op.runWith(fg)

    def runWith[M2[_] <: CopK[_], G[_]: Monad](i: Interpreter[M2, G])(implicit s: SubCop[M, M2]): G[A] =
      op.runWith(i)
  }
}

abstract class ComposeFree[D <: DSL, M[_] <: CopK[_]](M: DSL.Make[D, M])
extends ComposeFreeLP0[D, M](M) { self =>
  implicit def liftFA[F[_], A](fa: F[A])(implicit s: SubCop[In1[F, ?], M]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A]): Composed[A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[RecNode[M, ?], A]): Composed[A] = nfa.op

  def |>[A](f: ComposeFree[D, M] => A): A = f(self)
}

object ComposeFree {
  def apply[D <: DSL](implicit toCop: ToCopK[D]): ComposeFree[D, toCop.Cop] =
    new ComposeFree[D, toCop.Cop](DSL.Make[D]) {}
}

object syntax extends ComposeOps
