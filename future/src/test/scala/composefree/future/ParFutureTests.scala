package composefree
package future

import cats.kernel.Eq
import cats.laws.discipline.{ApplicativeErrorTests, ParallelTests}
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.syntax.eq._
import org.scalacheck.{Arbitrary, Properties}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object ParFutureTestsHelper {
  val timeout = 3.seconds

  def futureEither[A](f: Future[A]): Future[Either[Throwable, A]] =
    f.transformWith(t => Future.successful(t.toEither))

  implicit def eqFuture[A: Eq]: Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(fx: Future[A], fy: Future[A]): Boolean =
        Await.result(futureEither(fx).zip(futureEither(fy)).map { case (tx, ty) => tx === ty }, timeout)
    }

  implicit def arbParFuture[A: Arbitrary]: Arbitrary[ParFuture[A]] =
    ParFuture.deriving

  implicit def eqParFuture[A: Eq]: Eq[ParFuture[A]] =
    ParFuture.deriving

  implicit val throwableEq: Eq[Throwable] =
    Eq.by[Throwable, String](_.toString)

  // Need non-fatal Throwables for Future recoverWith/handleError
  implicit val nonFatalArbitrary: Arbitrary[Throwable] =
    Arbitrary(Arbitrary.arbitrary[Exception].map(identity))
}

object ParFutureTests extends Properties("ParFuture") {
  import ParFutureTestsHelper._

  include(ApplicativeErrorTests[ParFuture, Throwable].applicative[Int, Int, Int].all)
  include(ParallelTests[Future, ParFuture].parallel[Int, String].all)

  include(MonoidTests[ParFuture[Int]].monoid.all)
  include(SemigroupTests[ParFuture[Int]].semigroup.all)
}
