package composefree.example

import cats.~>
import cats.effect.IO
import cats.free.Free
import composefree.syntax.lift._
import scala.concurrent.Future

object numbers {
  sealed trait Numbers[A]
  case class set(i: Int) extends Numbers[Unit]
  case class get() extends Numbers[Int]
  case class add(i: Int) extends Numbers[Int]
  case class minus(i: Int) extends Numbers[Int]

  def update(fn: Int => Int): Free[Numbers, Int] =
    for {
      c <- get()
      r = fn(c)
      _ <- set(r)
    } yield r

  object RunNumbersFuture {
    def apply(): Numbers ~> Future = new (Numbers ~> Future) {
      var x = 0
      def apply[A](n: Numbers[A]) = n match {
        case set(i) => Future.successful({ x = i })
        case get() => Future.successful(x)
        case add(i) => Future.successful({ x = x+i; x })
        case minus(i) => Future.successful({ x = x-i; x })
      }
    }
  }

  object RunNumbersIO {
    def apply(): Numbers ~> IO = new (Numbers ~> IO) {
      var x = 0
      def apply[A](n: Numbers[A]) = n match {
        case set(i) => IO.pure({ x = i })
        case get() => IO.pure(x)
        case add(i) => IO.pure({ x = x+i; x })
        case minus(i) => IO.pure({ x = x-i; x })
      }
    }
  }
}
