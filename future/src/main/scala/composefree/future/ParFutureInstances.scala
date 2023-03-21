package composefree
package future

import cats.{~>, Applicative, ApplicativeError, Apply, Monad, Monoid, Parallel, Semigroup}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

private[future] trait ParFutureInstances0 extends ParFutureInstances1 {
  implicit def parallelForParFuture(implicit ec: ExecutionContext): Parallel.Aux[Future, ParFuture] =
    new Parallel[Future] {
      type F[a] = ParFuture[a]
      lazy val monad: Monad[Future] = cats.instances.future.catsStdInstancesForFuture
      lazy val applicative: Applicative[ParFuture] = ParFuture.applicativeErrorForParFuture
      lazy val parallel: Future ~> ParFuture = new (Future ~> ParFuture) {
        def apply[A](f: Future[A]): ParFuture[A] = ParFuture(f)
      }
      lazy val sequential: ParFuture ~> Future = new (ParFuture ~> Future) {
        def apply[A](f: ParFuture[A]): Future[A] = f.run
      }
    }

  implicit def applicativeErrorForParFuture(implicit ec: ExecutionContext): ApplicativeError[ParFuture, Throwable] =
    new ApplicativeError[ParFuture, Throwable] {
      def pure[A](a: A): ParFuture[A] =
        ParFuture(Future.successful(a))

      def ap[A, B](fab: ParFuture[A => B])(fa: ParFuture[A]): ParFuture[B] = {
        val p = Promise[B]()
        val r = new AtomicReference[(Try[A], Try[A => B])]((null, null))

        def tryDone(ta: Try[A], tab: Try[A => B]): (Try[A], Try[A => B]) = {
          if (ta != null && tab != null)
            p.complete(tab.flatMap(ta.map(_)))

          (ta, tab)
        }

        fab.run.onComplete { tab =>
          r.getAndUpdate { case (ta, _) =>
            tryDone(ta, tab)
          }
        }

        fa.run.onComplete { ta =>
          r.getAndUpdate { case (_, tab) =>
            tryDone(ta, tab)
          }
        }

        ParFuture(p.future)
      }

      def raiseError[A](e: Throwable): ParFuture[A] =
        ParFuture(Future.failed(e))

      def handleErrorWith[A](fa: ParFuture[A])(f: Throwable => ParFuture[A]): ParFuture[A] =
        ParFuture(fa.run.recoverWith { case t => f(t).run })
    }
}

sealed private[future] trait ParFutureInstances1 extends ParFutureInstances2 {
  implicit def monoidForParFuture[A](implicit a: Monoid[A], ec: ExecutionContext): Monoid[ParFuture[A]] =
    Applicative.monoid[ParFuture, A](ParFuture.applicativeErrorForParFuture, a)
}

sealed private[future] trait ParFutureInstances2 {
  implicit def semigroupForParFuture[A](implicit a: Semigroup[A], ec: ExecutionContext): Semigroup[ParFuture[A]] =
    Apply.semigroup[ParFuture, A](ParFuture.applicativeErrorForParFuture, a)
}
