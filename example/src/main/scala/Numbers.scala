package composefree.example

import composefree.example.examplecompose._
import scala.concurrent.Future
import scalaz.~>

object numbers {
  sealed trait Numbers[A]
  case class set(i: Int) extends Numbers[Unit]
  case class get() extends Numbers[Int]
  case class add(i: Int) extends Numbers[Int]
  case class minus(i: Int) extends Numbers[Int]

  def update(fn: Int => Int): Composed[Int] =
    for {
      c <- get()
      r = fn(c)
      _ <- set(r)
    } yield r

  object RunNumbers extends (Numbers ~> Future) {
    var x = 0
    def apply[A](n: Numbers[A]) = n match {
      case set(i) => Future.successful(x = i)
      case get() => Future.successful(x)
      case add(i) => Future.successful({ x = x+i; x })
      case minus(i) => Future.successful({ x = x-i; x })
    }
  }
}
