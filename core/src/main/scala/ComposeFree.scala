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
      p.foldMap(new (G ~> FreeApplicative[GG, *]) {
        def apply[a](g: G[a]) = FreeApplicative.lift(i.inj(g))
      })

    def asM[GG[_]](implicit i: InjectK[G, GG]): Free[GG, A] =
      p.foldMap(new (G ~> Free[GG, *]) {
        def apply[a](g: G[a]) = Free.liftF(i.inj(g))
      })

    def asM: Free[G, A] =
      p.foldMap(new (G ~> Free[G, *]) {
        def apply[a](g: G[a]) = Free.liftF(g)
      })
  }

  implicit class ComposeFreeOps[M[_], A](p: Free[M, A]) {
    def runM[T[_]: Monad](i: M ~> T): T[A] =
      p.foldMap(i)

    def as[G[_]](implicit i: InjectK[M, G]): Free[G, A] =
      p.foldMap(new (M ~> Free[G, *]) {
        def apply[a](m: M[a]) = Free.liftF(i.inj(m))
      })
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
  final type RecNode[A] = composefree.RecNode[M, A]
  final type RecProg[A] = composefree.RecProg[M, A]
  final type RecApProg[A] = composefree.RecApProg[M, A]
  final type Composed[A] = composefree.Composed[M, A]

  final type ComposeNode[A] = composefree.ComposeNode[M, A]
  final type MNode[A] = composefree.MNode[M, A]
  final val MNode: composefree.MNode.type = composefree.MNode
  final type ANode[A] = composefree.ANode[M, A]
  final val ANode: composefree.ANode.type = composefree.ANode

  class RecInterp[G[_]: Monad](mg: M ~> G) extends (ComposeNode ~> G) { self =>
    def interp: RecNode ~> G =
      new (RecNode ~> G) {
        def apply[A](n: RecNode[A]) = RecNode.fold(n)(self, mg)
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
          P.applicative.pure(a)

        override def ap[A, B](pab: P[A => B])(pa: P[A]): P[B] =
          P.applicative.ap(pab)(pa)

        def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] =
          P.parallel(M.flatMap(P.sequential(fa))(a => P.sequential(f(a))))

        def tailRecM[A, B](a: A)(f: A => P[Either[A, B]]): P[B] =
          P.parallel(M.tailRecM(a)(x => P.sequential(f(x))))
      }

      lazy val mp: M ~> P = mg.andThen(P.parallel)

      lazy val rnp: RecNode ~> P = new (RecNode ~> P) {
        def apply[A](n: RecNode[A]) = RecNode.fold(n)(cnp, mp)
      }

      lazy val cnp: ComposeNode ~> P = new (ComposeNode ~> P) {
        def apply[A](n: ComposeNode[A]) = n match {
          case MNode(mn) => P.parallel(mn.foldMap(interp))
          case ANode(an) => an.foldMap(rnp)(P.applicative)
        }
      }

      override def apply[A](cn: ComposeNode[A]): G[A] = cn match {
        case MNode(mn) => mn.foldMap(interp)
        case ANode(an) => P.sequential(an.foldMap(rnp))
      }
    }

  implicit class FOps[F[_], A](fa: F[A])(implicit i: InjectK[F, M]) {
    def op: RecProg[A] = RecProg.liftM(i.inj(fa))
    def opAp: RecApProg[A] = RecApProg.liftM(i.inj(fa))
  }

  implicit class MOps[A](ma: M[A]) {
    def op: RecProg[A] = RecProg.liftM(ma)
    def opAp: RecApProg[A] = RecApProg.liftM(ma)
  }

  implicit class ProgOps[A](pa: RecProg[A]) {
    def op: RecProg[A] = pa
    def opAp: RecApProg[A] = RecApProg.liftCN(MNode(pa))

    def runWithSeq[G[_]: Monad](mg: M ~> G): G[A] =
      pa.foldMap(seqRecInterp(mg).interp)

    def runWith[G[_]: Monad: Parallel](mg: M ~> G): G[A] =
      pa.foldMap(recInterp(mg).interp)
  }

  implicit class ApProgOps[A](apa: RecApProg[A]) {
    def op: RecProg[A] = RecProg.liftCN(ANode(apa))
    def opAp: RecApProg[A] = apa
  }

  implicit class ComposeNodeOps[A](nfa: ComposeNode[A]) {
    def op: RecProg[A] = RecProg.liftCN(nfa)
    def opAp: RecApProg[A] = RecApProg.liftCN(nfa)

    def runWithSeq[G[_]: Monad](mg: M ~> G): G[A] =
      nfa.op.foldMap(seqRecInterp(mg).interp)

    def runWith[G[_]: Monad: Parallel](mg: M ~> G): G[A] =
      nfa.opAp.foldMap(recInterp(mg).interp)
  }

  implicit class FreeApplicativeOps[A](p: FreeApplicative[M, A]) {
    def op: Composed[A] =
      p.foldMap(new (M ~> Composed) {
        def apply[a](m: M[a]) = m.op
      })

    def opAp: RecApProg[A] =
      p.foldMap(new (M ~> RecApProg) {
        def apply[a](m: M[a]) = m.opAp
      })
  }

  implicit class ComposeFreeOpOps[A](p: Free[M, A]) {
    def op: Composed[A] =
      p.foldMap(new (M ~> Composed) {
        def apply[a](m: M[a]) = m.op
      })
  }

  implicit def f2fnf[F[_], A](fa: F[A])(implicit i: InjectK[F, M]): Composed[A] = fa.op
  implicit def m2mnm[A](ma: M[A]): Composed[A] = ma.op
  implicit def nf2fnf[A](nfa: ComposeNode[A]): Composed[A] = nfa.op
}

object syntax extends ComposeOps
