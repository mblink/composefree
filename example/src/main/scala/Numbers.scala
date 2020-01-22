package composefree.example

import cats.~>
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

  val runNumbers = Lambda[Numbers ~> Future] { n =>
    var x = 0
    n match {
      case set(i) => Future.successful({ x = i })
      case get() => Future.successful(x)
      case add(i) => Future.successful({ x = x+i; x })
      case minus(i) => Future.successful({ x = x-i; x })
    }
  }
}
