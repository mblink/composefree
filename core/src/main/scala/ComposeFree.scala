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

sealed abstract class ComposeFreeLP1[D <: DSL, M[_] <: CopK[_]](val M: DSL.Make[D, M]) extends ComposeOps {
  type RecProg[A] = Free[RecNode[M, ?], A]
  type RecApProg[A] = FreeApplicative[RecNode[M, ?], A]
  type Composed[A] = RecProg[A]

  protected lazy val mToRecNode = Lambda[M ~> RecNode[M, ?]](RecNode(_))
  protected lazy val mToRecProg = Lambda[M ~> RecProg](m => Free.liftF(mToRecNode(m)))
  protected lazy val mToRecApProg = Lambda[M ~> RecApProg](m => FreeApplicative.lift(mToRecNode(m)))

  protected def fToRecNode[F[_]](implicit s: SubCop[In1[F, ?], M]): F ~> RecNode[M, ?] =
    Lambda[F ~> RecNode[M, ?]](x => mToRecNode(s(In1(x))))

  protected def freeToFreeAp[F[_] <: CopK[_]]: Free[RecNode[F, ?], ?] ~> FreeApplicative[RecNode[F, ?], ?] =
    Lambda[Free[RecNode[F, ?], ?] ~> FreeApplicative[RecNode[F, ?], ?]](f => FreeApplicative.lift(RecNode(f)))

  protected def freeApToFree[F[_] <: CopK[_]]: FreeApplicative[RecNode[F, ?], ?] ~> Free[RecNode[F, ?], ?] =
    Lambda[FreeApplicative[RecNode[F, ?], ?] ~> Free[RecNode[F, ?], ?]](f => Free.liftF(RecNode(f)))

  protected def otherCNToThis[F[_] <: CopK[_]](implicit s: SubCop[F, M]): ComposeNode[RecNode[F, ?], ?] ~> ComposeNode[RecNode[M, ?], ?] =
    Lambda[ComposeNode[RecNode[F, ?], ?] ~> ComposeNode[RecNode[M, ?], ?]](_.run.fold(
      m => ComposeNode(m.mapK(otherRNToThis[F])),
      a => ComposeNode(a.compile(otherRNToThis[F]))))

  protected def otherRNToThis[F[_] <: CopK[_]](implicit s: SubCop[F, M]): RecNode[F, ?] ~> RecNode[M, ?] =
    Lambda[RecNode[F, ?] ~> RecNode[M, ?]](_.run.fold(
      cn => RecNode(otherCNToThis.apply(cn)),
      fa => RecNode(s(fa))))
}

sealed abstract class ComposeFreeLP0[D <: DSL, M[_] <: CopK[_]](M: DSL.Make[D, M])(implicit T: ToCopK.Aux[D, M])
extends ComposeFreeLP1[D, M](M) {
  private abstract class RecInterp[G[_]] extends (ComposeNode[RecNode[M, ?], ?] ~> G) {
    def interp: (RecNode[M, ?] ~> G)
  }

  private def recInterp[G[_]](fg: (M ~> G))(implicit M: Monad[G]): RecInterp[G] =
    new RecInterp[G] { self =>
      lazy val interp: (RecNode[M, ?] ~> G) = new (RecNode[M, ?] ~> G) {
        def apply[A](cp: RecNode[M, A]) = cp.run.fold(self(_), fg(_))
      }

      def apply[A](cn: ComposeNode[RecNode[M, ?], A]) = cn.run.fold(_.foldMap(interp), _.foldMap(interp))
    }

  implicit class FOps[F[_], A](fa: F[A])(implicit s: SubCop[In1[F, ?], M]) {
    def op: RecProg[A] = fa.freek[D].mapK(mToRecNode)
    def opAp: RecApProg[A] = fa.freekAp[D].compile(mToRecNode)
  }

  implicit class MOps[A](ma: M[A]) {
    def op: RecProg[A] = mToRecProg(ma)
    def opAp: RecApProg[A] = mToRecApProg(ma)
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

  implicit class ExpandFreeFOps[F[_] <: CopK[_], A](fa: Free[RecNode[F, ?], A])(implicit s: SubCop[F, M]) {
    def op: RecProg[A] = fa.mapK(otherRNToThis[F])
    def opAp: RecApProg[A] = freeToFreeAp(fa.mapK(otherRNToThis[F]))
  }

  implicit class ExpandFreeApplicativeFOps[F[_] <: CopK[_], A](fa: FreeApplicative[RecNode[F, ?], A])(implicit s: SubCop[F, M]) {
    def op: RecProg[A] = freeApToFree(fa.compile(otherRNToThis[F]))
    def opAp: RecApProg[A] = fa.compile(otherRNToThis[F])
  }

  implicit class RecProgOps[A](pa: RecProg[A]) {
    def op: RecProg[A] = pa
    def opAp: RecApProg[A] = freeToFreeAp(pa)

    def runWith[G[_]: Monad](fg: (M ~> G)): G[A] =
      pa.foldMap(recInterp(fg).interp)

    def runWith[M2[_] <: CopK[_], G[_]: Monad](i: Interpreter[M2, G])(implicit s: SubCop[M, M2]): G[A] =
      runWith(Lambda[M ~> G](m => i.nat(s(m))))
  }

  implicit class RecApProgOps[A](apa: RecApProg[A]) {
    def op: RecProg[A] = freeApToFree(apa)
    def opAp: RecApProg[A] = apa
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

abstract class ComposeFree[D <: DSL, M[_] <: CopK[_]](M: DSL.Make[D, M])(implicit T: ToCopK.Aux[D, M])
extends ComposeFreeLP0[D, M](M) {
  implicit def liftFA[F[_], A](fa: F[A])(implicit s: SubCop[In1[F, ?], M]): Composed[A] = fa.op
  implicit def expandFreeFA[F[_] <: CopK[_], A](fa: Free[RecNode[F, ?], A])(implicit s: SubCop[F, M]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A]): Composed[A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[RecNode[M, ?], A]): Composed[A] = nfa.op
}

object ComposeFree {
  def apply[D <: DSL](implicit toCop: ToCopK[D]): ComposeFree[D, toCop.Cop] =
    new ComposeFree[D, toCop.Cop](DSL.Make[D])(toCop) {}
}

object syntax extends ComposeOps
