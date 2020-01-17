import cats.~>
import cats.free.{Free, FreeApplicative}
import cats.syntax.either._
import freek._
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

package object composefree extends composefree.ComposeFreeConversions {
  @newtype class ComposeNode[F[_], A](val run: Either[Free[F, A], FreeApplicative[F, A]])

  object ComposeNode {
    def apply[F[_], A](fa: Free[F, A]): ComposeNode[F, A] =
      fa.asLeft[FreeApplicative[F, A]].coerce

    def apply[F[_], A](fa: FreeApplicative[F, A]): ComposeNode[F, A] =
      fa.asRight[Free[F, A]].coerce
  }

  @newtype class RecNode[F[_] <: CopK[_], A](val run: Either[ComposeNode[RecNode[F, ?], A], F[A]])

  object RecNode {
    def apply[F[_] <: CopK[_], A](fa: F[A]): RecNode[F, A] =
      fa.asRight[ComposeNode[RecNode[F, ?], A]].coerce

    def apply[F[_] <: CopK[_], A](free: Free[RecNode[F, ?], A]): RecNode[F, A] =
      ComposeNode(free).asLeft[F[A]].coerce

    def apply[F[_] <: CopK[_], A](freeAp: FreeApplicative[RecNode[F, ?], A]): RecNode[F, A] =
      ComposeNode(freeAp).asLeft[F[A]].coerce

    def apply[F[_] <: CopK[_], A](cn: ComposeNode[RecNode[F, ?], A]): RecNode[F, A] =
      cn.asLeft[F[A]].coerce
  }

  type Composed[D <: DSL, A] = RecProg[D, A]
  object Composed {
    type Aux[D <: DSL, M0[_] <: CopK[_], A] = RecProg[D, A] { type M[a] = M0[a] }
  }

  private[composefree] def mToRecNode[M[_] <: CopK[_]]: M ~> RecNode[M, ?] =
    Lambda[M ~> RecNode[M, ?]](RecNode(_))

  private[composefree] def mToRecProg[D <: DSL, M[_] <: CopK[_]]: M ~> RecProg.Aux[D, M, ?] =
    Lambda[M ~> RecProg.Aux[D, M, ?]](m => RecProg.pAp[M, D](Free.liftF(mToRecNode(m))))

  private[composefree] def mToRecApProg[D <: DSL, M[_] <: CopK[_]]: M ~> RecApProg.Aux[D, M, ?] =
    Lambda[M ~> RecApProg.Aux[D, M, ?]](m => RecApProg.pAp[M, D](FreeApplicative.lift(mToRecNode(m))))

  private[composefree] def fToRecNode[F[_], D <: DSL, M[_] <: CopK[_]](implicit s: SubDSL1.Aux[F, D, M]): F ~> RecNode[M, ?] =
    Lambda[F ~> RecNode[M, ?]](x => mToRecNode(s.sub(In1(x))))

  private[composefree] def freeToFreeAp[F[_] <: CopK[_]]: Free[RecNode[F, ?], ?] ~> FreeApplicative[RecNode[F, ?], ?] =
    Lambda[Free[RecNode[F, ?], ?] ~> FreeApplicative[RecNode[F, ?], ?]](f => FreeApplicative.lift(RecNode(f)))

  private[composefree] def freeApToFree[F[_] <: CopK[_]]: FreeApplicative[RecNode[F, ?], ?] ~> Free[RecNode[F, ?], ?] =
    Lambda[FreeApplicative[RecNode[F, ?], ?] ~> Free[RecNode[F, ?], ?]](f => Free.liftF(RecNode(f)))

  private[composefree] def otherCNToThis[F[_] <: CopK[_], D <: DSL, M[_] <: CopK[_]](implicit s: SubCop[F, M]): ComposeNode[RecNode[F, ?], ?] ~> ComposeNode[RecNode[M, ?], ?] =
    Lambda[ComposeNode[RecNode[F, ?], ?] ~> ComposeNode[RecNode[M, ?], ?]](_.run.fold(
      m => ComposeNode(m.mapK(otherRNToThis[F, D, M])),
      a => ComposeNode(a.compile(otherRNToThis[F, D, M]))))

  private[composefree] def otherRNToThis[F[_] <: CopK[_], D <: DSL, M[_] <: CopK[_]](implicit s: SubCop[F, M]): RecNode[F, ?] ~> RecNode[M, ?] =
    Lambda[RecNode[F, ?] ~> RecNode[M, ?]](_.run.fold(
      cn => RecNode(otherCNToThis.apply(cn)),
      fa => RecNode(s(fa))))
}
