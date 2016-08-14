package composefree.example

import composefree.syntax.lift._
import scalaz.Trampoline._
import scalaz.{Free, ~>}

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

  object RunNumbers {

    def apply() = new (Numbers ~> Free.Trampoline) {
      var x = 0
      def apply[A](n: Numbers[A]) = n match {
        case set(i) => delay(x = i)
        case get() => delay(x)
        case add(i) => delay({ x = x+i; x })
        case minus(i) => delay({ x = x-i; x })
      }
    }
  }
}
