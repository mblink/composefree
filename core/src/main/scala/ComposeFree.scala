package composefree

import cats.{~>, Applicative, InjectK, Monad, Parallel}
import cats.data.EitherK
import cats.evidence.As
import cats.free.{Free, FreeApplicative}

object puredsl {
  sealed trait PureOp[A]
  case class pure[A](a: A) extends PureOp[A]
}

trait LPSyntax {
  implicit class CommandOps[M[_], A](m: M[A]) {
    def as[G[_]](implicit ev: As[M[A], G[A]]): G[A] = ev(m)
  }
}

trait ComposeOps extends LPSyntax {
  implicit class NTOps[F[_], O[_]](fo: F ~> O) {
    def |:[G[_]](go: G ~> O): EitherK[G, F, *] ~> O =
      go.or(fo)
  }

  implicit class ComposeFreeApplicativeOps[G[_], A](p: FreeApplicative[G, A]) {
    def runAp[T[_]: Applicative](i: G ~> T): T[A] =
      p.foldMap(i)

    def as[GG[_]](implicit i: InjectK[G, GG]): FreeApplicative[GG, A] =
      p.foldMap(Lambda[G ~> FreeApplicative[GG, *]](g => FreeApplicative.lift(i.inj(g))))

    def asM[GG[_]](implicit i: InjectK[G, GG]): Free[GG, A] =
      p.foldMap(Lambda[G ~> Free[GG, *]](g => Free.liftF(i.inj(g))))

    def asM: Free[G, A] =
      p.foldMap(Lambda[G ~> Free[G, *]](Free.liftF(_)))
  }

  implicit class ComposeFreeOps[M[_], A](p: Free[M, A]) {
    def runM[T[_]: Monad](i: M ~> T): T[A] =
      p.foldMap(i)

    def as[G[_]](implicit i: InjectK[M, G]): Free[G, A] =
      p.foldMap(Lambda[M ~> Free[G, *]](m => Free.liftF(i.inj(m))))
  }

  object lift {
    implicit class ToFreeOps[M[_], A](m: M[A]) {
      def liftF: Free[M, A] =
        Free.liftF(m)

      def ap: FreeApplicative[M, A] =
        FreeApplicative.lift(m)
    }

    implicit def liftF[M[_], A](m: M[A]): Free[M, A] = m.liftF
  }
}

trait ComposeFree[M[_]] extends ComposeOps {
  type RecNode[A] = EitherK[ComposeNode, M, A]
  type RecProg[A] = Free[RecNode, A]
  type RecApProg[A] = FreeApplicative[RecNode, A]

  type Composed[A] = RecProg[A]

  sealed trait ComposeNode[A]
  case class MNode[A](run: Free[RecNode, A]) extends ComposeNode[A]
  case class ANode[A](run: FreeApplicative[RecNode, A]) extends ComposeNode[A]

  class RecInterp[G[_]: Monad](mg: M ~> G) extends (ComposeNode ~> G) { self =>
    def interp: RecNode ~> G =
      Lambda[RecNode ~> G](_.fold(self, mg))

    def apply[A](cn: ComposeNode[A]): G[A] = cn match {
      case MNode(mn) => mn.foldMap(interp)
      case ANode(apn) => apn.foldMap(interp)
    }
  }

  def seqRecInterp[G[_]: Monad](mg: M ~> G): RecInterp[G] =
    new RecInterp[G](mg) {}

  def recInterp[G[_]](mg: M ~> G)(implicit M: Monad[G], P: Parallel[G]): RecInterp[G] =
    new RecInterp[G](mg) { self =>
      type P[A] = P.F[A]

      implicit val PMonad: Monad[P] = new Monad[P] {
        def pure[A](a: A): P[A] =
          P.applicative.pure(a)

        override def ap[A, B](pab: P[A => B])(pa: P[A]): P[B] =
          P.applicative.ap(pab)(pa)

        def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] =
          P.parallel(M.flatMap(P.sequential(fa))(a => P.sequential(f(a))))

        def tailRecM[A, B](a: A)(f: A => P[Either[A, B]]): P[B] =
          P.parallel(M.tailRecM(a)(x => P.sequential(f(x))))
      }

      lazy val mp: M ~> P = mg.andThen(P.parallel)

      lazy val rnp: RecNode ~> P = Lambda[RecNode ~> P](_.fold(cnp, mp))

      lazy val cnp: ComposeNode ~> P = Lambda[ComposeNode ~> P](_ match {
        case MNode(mn) => P.parallel(mn.foldMap(interp))
        case ANode(an) => an.foldMap(rnp)(P.applicative)
      })

      override def apply[A](cn: ComposeNode[A]): G[A] = cn match {
        case MNode(mn) => mn.foldMap(interp)
        case ANode(an) => P.sequential(an.foldMap(rnp))
      }
    }

  implicit class FOps[F[_], A](fa: F[A])(implicit i: InjectK[F, M]) {
    def op: RecProg[A] =
      Free.liftF(EitherK.right[ComposeNode](i.inj(fa)): RecNode[A])

    def opAp: RecApProg[A] =
      FreeApplicative.lift(EitherK.right[ComposeNode](i.inj(fa)): RecNode[A])
  }

  implicit class MOps[A](ma: M[A]) {
    def op: RecProg[A] =
      Free.liftF(EitherK.right[ComposeNode](ma): RecNode[A])

    def opAp: RecApProg[A] =
      FreeApplicative.lift(EitherK.right[ComposeNode](ma): RecNode[A])
  }

  implicit class ProgOps[A](pa: RecProg[A]) {
    def op: RecProg[A] =
      pa

    def opAp: RecApProg[A] =
      FreeApplicative.lift(EitherK.left[M](MNode(pa)): RecNode[A])

    def runWithSeq[G[_]: Monad](mg: M ~> G): G[A] =
      pa.foldMap(seqRecInterp(mg).interp)

    def runWith[G[_]: Monad: Parallel](mg: M ~> G): G[A] =
      pa.foldMap(recInterp(mg).interp)
  }

  implicit class ApProgOps[A](apa: RecApProg[A]) {
    def op: RecProg[A] =
      Free.liftF(EitherK.left[M](ANode(apa)): RecNode[A])

    def opAp: RecApProg[A] =
      apa
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[A]) {
    def op: RecProg[A] =
      Free.liftF(EitherK.left[M](nfa): RecNode[A])

    def opAp: RecApProg[A] =
      FreeApplicative.lift(EitherK.left[M](nfa): RecNode[A])

    def runWithSeq[G[_]: Monad](mg: M ~> G): G[A] =
      nfa.op.foldMap(seqRecInterp(mg).interp)

    def runWith[G[_]: Monad: Parallel](mg: M ~> G): G[A] =
      nfa.opAp.foldMap(recInterp(mg).interp)
  }

  implicit class FreeApplicativeOps[A](p: FreeApplicative[M, A]) {
    def op: Composed[A] =
      p.foldMap(Lambda[M ~> Composed](_.op))

    def opAp: RecApProg[A] =
      p.foldMap(Lambda[M ~> RecApProg](_.opAp))
  }

  implicit class ComposeFreeOpOps[A](p: Free[M, A]) {
    def op: Composed[A] =
      p.foldMap(Lambda[M ~> Composed](_.op))
  }

  implicit def f2fnf[F[_], A](fa: F[A])(implicit i: InjectK[F, M]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A]): Composed[A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[A]): Composed[A] = nfa.op
}

object syntax extends ComposeOps
