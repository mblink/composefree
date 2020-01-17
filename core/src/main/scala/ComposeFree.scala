package composefree

import cats.{~>, Applicative, Monad}
import cats.free.{Free, FreeApplicative}
import cats.evidence.As
import freek._

object puredsl {
  sealed trait PureOp[A]
  case class pure[A](a: A) extends PureOp[A]
}

trait ComposeOps {
  implicit class FAsOps[F[_], A](fa: F[A]) {
    def as[G[_]](implicit ev: As[F[A], G[A]]): G[A] = ev(fa)
  }

  object lift {
    implicit class ToFreeOps[M[_], A](m: M[A]) {
      def liftF: Free[M, A] = Free.liftF(m)
      def ap: FreeApplicative[M, A] = FreeApplicative.lift(m)
    }

    implicit def liftF[M[_], A](m: M[A]): Free[M, A] = m.liftF
  }
}

sealed abstract class RecProg[D <: DSL, A] {
  type M[_] <: CopK[_]
  def run: Free[RecNode[M, ?], A]

  def map[B](f: A => B): RecProg.Aux[D, M, B] =
    RecProg(run.map(f))

  def flatMap[B](f: A => RecProg.Aux[D, M, B]): RecProg.Aux[D, M, B] =
    RecProg(run.flatMap(f(_).run))
}

object RecProg {
  type Aux[D <: DSL, M0[_] <: CopK[_], A] = RecProg[D, A] {
    type M[a] = M0[a]
  }

  trait PartialAp[D <: DSL, M0[_] <: CopK[_]] {
    def apply[A](fa: Free[RecNode[M0, ?], A]): Aux[D, M0, A] =
      new RecProg[D, A] {
        type M[a] = M0[a]
        val run = fa
      }
  }

  def pAp[M[_] <: CopK[_], D <: DSL]: PartialAp[D, M] = new PartialAp[D, M] {}

  def apply[D <: DSL, M0[_] <: CopK[_], A](fa: Free[RecNode[M0, ?], A]): Aux[D, M0, A] =
    new RecProg[D, A] {
      type M[a] = M0[a]
      val run = fa
    }

  implicit def monad[M[_] <: CopK[_], D <: DSL]: Monad[Aux[D, M, ?]] = new Monad[Aux[D, M, ?]] {
    def pure[A](a: A): Aux[D, M, A] =
      apply(Free.pure[RecNode[M, ?], A](a))

    def flatMap[A, B](fa: Aux[D, M, A])(f: A => Aux[D, M, B]): Aux[D, M, B] =
      apply(fa.run.flatMap(f(_).run))

    def tailRecM[A, B](a: A)(f: A => Aux[D, M, Either[A, B]]): Aux[D, M, B] =
      apply(f(a).run.flatMap(_.fold(tailRecM(_)(f).run, Free.pure[RecNode[M, ?], B](_))))
  }
}

sealed abstract class RecApProg[D <: DSL, A] {
  type M[_] <: CopK[_]
  def run: FreeApplicative[RecNode[M, ?], A]
}

object RecApProg {
  type Aux[D <: DSL, M0[_] <: CopK[_], A] = RecApProg[D, A] {
    type M[a] = M0[a]
  }

  trait PartialAp[D <: DSL, M0[_] <: CopK[_]] {
    def apply[A](fa: FreeApplicative[RecNode[M0, ?], A]): Aux[D, M0, A] =
      new RecApProg[D, A] {
        type M[a] = M0[a]
        val run = fa
      }
  }

  def pAp[M[_] <: CopK[_], D <: DSL]: PartialAp[D, M] = new PartialAp[D, M] {}

  def apply[D <: DSL, M0[_] <: CopK[_], A](fa: FreeApplicative[RecNode[M0, ?], A]): Aux[D, M0, A] =
    new RecApProg[D, A] {
      type M[a] = M0[a]
      val run = fa
    }

  implicit def applicative[M[_] <: CopK[_], D <: DSL]: Applicative[Aux[D, M, ?]] = new Applicative[Aux[D, M, ?]] {
    def pure[A](a: A): Aux[D, M, A] =
      apply(FreeApplicative.pure[RecNode[M, ?], A](a))

    def ap[A, B](ff: Aux[D, M, A => B])(fa: Aux[D, M, A]): Aux[D, M, B] =
      apply(fa.run.ap(ff.run))
  }
}

trait ComposeFreeLP1[D <: DSL, M[_] <: CopK[_]] {
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

  implicit class RecProgOps[A](pa: RecProg.Aux[D, M, A]) {
    def op: RecProg.Aux[D, M, A] = pa
    def opAp: RecApProg.Aux[D, M, A] = RecApProg(freeToFreeAp(pa.run))

    def runWith[G[_]: Monad](fg: (M ~> G)): G[A] =
      pa.run.foldMap(recInterp(fg).interp)

    def runWith[M2[_] <: CopK[_], G[_]: Monad](i: Interpreter[M2, G])(implicit s: SubCop[M, M2]): G[A] =
      runWith(Lambda[M ~> G](m => i.nat(s(m))))
  }

  implicit class RecApProgOps[A](apa: RecApProg.Aux[D, M, A]) {
    def op: RecProg.Aux[D, M, A] = RecProg(freeApToFree(apa.run))
    def opAp: RecApProg.Aux[D, M, A] = apa
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[RecNode[M, ?], A]) {
    def op: RecProg.Aux[D, M, A] = RecProg(Free.liftF(RecNode(nfa)))
    def opAp: RecApProg.Aux[D, M, A] = RecApProg(FreeApplicative.lift(RecNode(nfa)))

    def runWith[G[_]: Monad](fg: (M ~> G)): G[A] =
      op.runWith(fg)

    def runWith[M2[_] <: CopK[_], G[_]: Monad](i: Interpreter[M2, G])(implicit s: SubCop[M, M2]): G[A] =
      op.runWith(i)
  }
}

trait ComposeFreeLP0[D <: DSL, M[_] <: CopK[_]] extends ComposeFreeLP1[D, M] {
  implicit class FOps[F[_], A](fa: F[A])(implicit s: SubDSL1.Aux[F, D, M]) {
    def op: RecProg.Aux[D, M, A] = RecProg(fa.freek[D].mapK(mToRecNode))
    def opAp: RecApProg.Aux[D, M, A] = RecApProg(fa.freekAp[D].compile(mToRecNode))
  }

  implicit class MOps[A](ma: M[A]) {
    def op: RecProg.Aux[D, M, A] = mToRecProg[D, M](ma)
    def opAp: RecApProg.Aux[D, M, A] = mToRecApProg[D, M](ma)
  }

  implicit class FreeFOps[F[_], A](fa: Free[F, A])(implicit s: SubDSL1.Aux[F, D, M]) {
    def op: RecProg.Aux[D, M, A] = RecProg(fa.mapK(fToRecNode[F, D, M]))
    def opAp: RecApProg.Aux[D, M, A] = RecApProg(freeToFreeAp(fa.mapK(fToRecNode[F, D, M])))
  }

  implicit class FreeMOps[A](fa: Free[M, A]) {
    def op: RecProg.Aux[D, M, A] = RecProg(fa.mapK(mToRecNode))
    def opAp: RecApProg.Aux[D, M, A] = RecApProg(freeToFreeAp(fa.mapK(mToRecNode)))
  }

  implicit class FreeApplicativeFOps[F[_], A](fa: FreeApplicative[F, A])(
    implicit s: SubDSL1.Aux[F, D, M]
  ) {
    def op: RecProg.Aux[D, M, A] = RecProg(freeApToFree(fa.compile(fToRecNode[F, D, M])))
    def opAp: RecApProg.Aux[D, M, A] = RecApProg(fa.compile(fToRecNode[F, D, M]))
  }

  implicit class FreeApplicativeMOps[A](p: FreeApplicative[M, A]) {
    def op: RecProg.Aux[D, M, A] = p.foldMap[RecProg.Aux[D, M, ?]](mToRecProg[D, M])(RecProg.monad[M, D])
    def opAp: RecApProg.Aux[D, M, A] = p.foldMap[RecApProg.Aux[D, M, ?]](mToRecApProg[D, M])(RecApProg.applicative[M, D])
  }

  implicit class RecProgFOps[F[_] <: CopK[_], D2 <: DSL, A](fa: RecProg.Aux[D2, F, A])(
    implicit s: SubCop[F, M]
  ) {
    def op: RecProg.Aux[D, M, A] = RecProg(fa.run.mapK(otherRNToThis[F, D, M]))
    def opAp: RecApProg.Aux[D, M, A] = RecApProg(freeToFreeAp(fa.run.mapK(otherRNToThis[F, D, M])))
  }

  implicit class RecApProgFOps[D2 <: DSL, F[_] <: CopK[_], A](fa: RecApProg.Aux[D2, F, A])(
    implicit s: SubCop[F, M]
  ) {
    def op: RecProg.Aux[D, M, A] = RecProg(freeApToFree(fa.run.compile(otherRNToThis[F, D, M])))
    def opAp: RecApProg.Aux[D, M, A] = RecApProg(fa.run.compile(otherRNToThis[F, D, M]))
  }
}

trait ComposeFree[D <: DSL, M[_] <: CopK[_]] extends ComposeFreeLP0[D, M] with ComposeOps {
  implicit def liftFA[F[_], A](fa: F[A])(
    implicit s: SubDSL1.Aux[F, D, M]
  ): Composed.Aux[D, M, A] = fa.op

  implicit def liftRecProgF[F[_] <: CopK[_], D2 <: DSL, A](fa: RecProg.Aux[D2, F, A])(
    implicit s: SubCop[F, M]
  ): Composed.Aux[D, M, A] = new RecProgFOps(fa).op

  implicit def m2mnm[A](ma: M[A]): Composed.Aux[D, M, A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[RecNode[M, ?], A]): Composed.Aux[D, M, A] = new ComposeNodeOps(nfa).op
}

trait ComposeFreeConversions {
  implicit def toFAsOps[M[_] <: CopK[_], D <: DSL, F[_], A](fa: F[A])(
    implicit c: ComposeFree[D, M]
  ): c.FAsOps[F, A] = new c.FAsOps[F, A](fa)

  implicit def toFOps[M[_] <: CopK[_], D <: DSL, F[_], A](fa: F[A])(
    implicit c: ComposeFree[D, M],
    s: SubDSL1.Aux[F, D, M]
  ): c.FOps[F, A] = new c.FOps[F, A](fa)

  implicit def toMOps[M[_] <: CopK[_], D <: DSL, A](ma: M[A])(
    implicit c: ComposeFree[D, M]
  ): c.MOps[A] = new c.MOps[A](ma)

  implicit def toFreeFOps[M[_] <: CopK[_], D <: DSL, F[_], A](fa: Free[F, A])(
    implicit c: ComposeFree[D, M],
    s: SubDSL1.Aux[F, D, M]
  ): c.FreeFOps[F, A] = new c.FreeFOps[F, A](fa)

  implicit def toFreeMOps[M[_] <: CopK[_], D <: DSL, A](fa: Free[M, A])(
    implicit c: ComposeFree[D, M]
  ): c.FreeMOps[A] = new c.FreeMOps[A](fa)

  implicit def toFreeApplicativeFOps[M[_] <: CopK[_], D <: DSL, F[_], A](fa: FreeApplicative[F, A])(
    implicit c: ComposeFree[D, M],
    s: SubDSL1.Aux[F, D, M]
  ): c.FreeApplicativeFOps[F, A] = new c.FreeApplicativeFOps[F, A](fa)

  implicit def toFreeApplicativeMOps[M[_] <: CopK[_], D <: DSL, A](fa: FreeApplicative[M, A])(
    implicit c: ComposeFree[D, M]
  ): c.FreeApplicativeMOps[A] = new c.FreeApplicativeMOps[A](fa)

  implicit def toRecProgFOps[M[_] <: CopK[_], F[_] <: CopK[_], D <: DSL, D2 <: DSL, A](fa: RecProg.Aux[D2, F, A])(
    implicit c: ComposeFree[D, M],
    s: SubCop[F, M]
  ): c.RecProgFOps[F, D2, A] = new c.RecProgFOps[F, D2, A](fa)

  implicit def toRecApProgFOps[M[_] <: CopK[_], F[_] <: CopK[_], D <: DSL, D2 <: DSL, A](fa: RecApProg.Aux[D2, F, A])(
    implicit c: ComposeFree[D, M],
    s: SubCop[F, M]
  ): c.RecApProgFOps[D2, F, A] = new c.RecApProgFOps[D2, F, A](fa)
}

object test {
  trait Using[D <: DSL, M[_] <: CopK[_]] {
    def dsl[A](f: ComposeFree[D, M] => A): A = f(new ComposeFree[D, M] {})
  }

  def using[D <: DSL](implicit t: ToCopK[D]): Using[D, t.Cop] = new Using[D, t.Cop] {}
}

object syntax extends ComposeOps
