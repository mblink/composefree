import cats.free.{Free, FreeApplicative}
import cats.syntax.either._
import freek._
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

package object composefree extends composefree.ComposedSyntax {
  type ComposeNode[F[_], A] = Either[Free[F, A], FreeApplicative[F, A]]

  object ComposeNode {
    def apply[F[_], A](fa: Free[F, A]): ComposeNode[F, A] =
      fa.asLeft[FreeApplicative[F, A]]

    def apply[F[_], A](fa: FreeApplicative[F, A]): ComposeNode[F, A] =
      fa.asRight[Free[F, A]]
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

  type Composed[F[_] <: CopK[_], A] = Free[RecNode[F, ?], A]
  type ComposedAp[F[_] <: CopK[_], A] = FreeApplicative[RecNode[F, ?], A]
}
