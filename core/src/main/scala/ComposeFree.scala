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

  class CNat[F[_], G[_], H[_]](f: F ~> H, g: G ~> H) extends (EitherK[F, G, ?] ~> H) {
    type From[A] = EitherK[F, G, A]
    override def or[I[_]](i: I ~> H) = new CNat[From, I, H](this, i)
    def apply[A](fa: From[A]): H[A] = fa.run.fold(f(_), g(_))
    def |:[I[_]](i: I ~> H) = new CNat[I, From, H](i, this)
  }

  implicit class NTOps[F[_], G[_]](nt: F ~> G) {
    def or[H[_]](ont: H ~> G) = new CNat[F, H, G](nt, ont)
    def |:[H[_]](ont: H ~> G) = new CNat[H, F, G](ont, nt)
  }

  implicit class ComposeFreeApplicativeOps[G[_], A](p: FreeApplicative[G, A]) {
    def runAp[T[_]: Applicative](i: (G ~> T)): T[A] = p.foldMap(i)
    def as[GG[_]](implicit i: InjectK[G, GG]): FreeApplicative[GG, A] = {
      type Out[B] = FreeApplicative[GG, B]
      p.foldMap(new (G ~> Out) { def apply[B](g: G[B]) = FreeApplicative.lift(i.inj(g)) })
    }
    def asM[GG[_]](implicit i: InjectK[G, GG]): Free[GG, A] = {
      type Out[B] = Free[GG, B]
      p.foldMap(new (G ~> Out) { def apply[B](g: G[B]) = Free.liftF(i.inj(g)) })
    }
    def asM: Free[G, A] = {
      type Out[B] = Free[G, B]
      p.foldMap(new (G ~> Out) { def apply[B](g: G[B]) = Free.liftF(g) })
    }
  }

  implicit class ComposeFreeOps[M[_], A](p: Free[M, A]) {
    def runM[T[_]: Monad](i: (M ~> T)): T[A] = p.foldMap(i)
    def as[G[_]](implicit i: InjectK[M, G]): Free[G, A] = {
      type Out[B] = Free[G, B]
      p.foldMap(new (M ~> Out) { def apply[B](m: M[B]) = Free.liftF(i.inj(m)) })
    }
  }

  object lift {
    implicit class ToFreeOps[M[_], A](m: M[A]) {
      def liftF: Free[M, A] = Free.liftF(m)
      def ap: FreeApplicative[M, A] = FreeApplicative.lift(m)
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

  abstract class RecInterp[G[_]: Monad](mg: M ~> G) extends (ComposeNode ~> G) { self =>
    def interp: RecNode ~> G = new (RecNode ~> G) {
      def apply[A](rna: RecNode[A]): G[A] = rna.fold(self, mg)
    }
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
          P.parallel(M.pure(a))

        def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] =
          P.parallel(M.flatMap(P.sequential(fa))(a => P.sequential(f(a))))

        def tailRecM[A, B](a: A)(f: A => P[Either[A, B]]): P[B] =
          P.parallel(M.tailRecM(a)(x => P.sequential(f(x))))
      }

      lazy val mp: M ~> P = new (M ~> P) {
        def apply[A3](ma: M[A3]): P[A3] = P.parallel(mg(ma))
      }

      lazy val rnp: RecNode ~> P = new (RecNode ~> P) {
        def apply[A2](rna: RecNode[A2]): P[A2] = rna.fold(cnp, mp)
      }

      lazy val cnp: ComposeNode ~> P = new (ComposeNode ~> P) {
        def apply[A](cn: ComposeNode[A]): P[A] = cn match {
          case MNode(mn) => mn.foldMap(rnp)
          case ANode(an) => an.foldMap(rnp)(P.applicative)
        }
      }

      override def apply[A](cn: ComposeNode[A]): G[A] = P.sequential(cnp(cn))
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
    def op: RecProg[A] = Free.liftF(EitherK.left[M](MNode(pa)): RecNode[A])
    def opAp: RecApProg[A] = FreeApplicative.lift(EitherK.left[M](MNode(pa)): RecNode[A])

    def runWithSeq[G[_]: Monad](fg: (M ~> G)): G[A] =
      pa.foldMap(seqRecInterp(fg).interp)

    def runWith[G[_]: Monad](mg: M ~> G)(implicit P: Parallel[G]): G[A] =
      pa.foldMap(recInterp(mg).interp)
  }

  implicit class ApProgOps[A](apa: RecApProg[A]) {
    def op: RecProg[A] = Free.liftF(EitherK.left[M](ANode(apa)): RecNode[A])
    def opAp: RecApProg[A] = FreeApplicative.lift(EitherK.left[M](ANode(apa)): RecNode[A])
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[A]) {
    def op: RecProg[A] = Free.liftF(EitherK.left[M](nfa): RecNode[A])
    def opAp: RecApProg[A] = FreeApplicative.lift(EitherK.left[M](nfa): RecNode[A])

    def runWithSeq[G[_]: Monad](mg: (M ~> G)): G[A] =
      nfa.op.foldMap(seqRecInterp(mg).interp)

    def runWith[G[_]: Monad](mg: M ~> G)(implicit P: Parallel[G]): G[A] =
      nfa.opAp.foldMap(recInterp(mg).interp)
  }

  implicit class FreeApplicativeOps[A](p: FreeApplicative[M, A]) {
    def op: Composed[A] =
      p.foldMap(new (M ~> Composed) { def apply[B](m: M[B]) = m.op })
    def opAp: RecApProg[A] =
      p.foldMap(new (M ~> RecApProg) { def apply[B](m: M[B]) = m.opAp })
  }

  implicit class ComposeFreeOpOps[A](p: Free[M, A]) {
    def op: Composed[A] =
      p.foldMap(new (M ~> Composed) { def apply[B](m: M[B]) = m.op })
  }

  implicit def f2fnf[F[_], A](fa: F[A])(implicit i: InjectK[F, M]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A]): Composed[A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[A]): Composed[A] = nfa.op
}

object syntax extends ComposeOps
