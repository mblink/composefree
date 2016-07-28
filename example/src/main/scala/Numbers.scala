package composefree.example

import scalaz.Id.Id
import scalaz.~>

object numbers {
  sealed trait Numbers[A]
  case class set(i: Int) extends Numbers[Unit]
  case class get() extends Numbers[Int]
  case class add(i: Int) extends Numbers[Int]
  case class minus(i: Int) extends Numbers[Int]

  object RunNumbers {

    def apply() = new (Numbers ~> Id) {
      var x = 0
      def apply[A](n: Numbers[A]) = n match {
        case set(i) => (x = i)
        case get() => x
        case add(i) => { x = x+i; x }
        case minus(i) => { x = x-i; x }
      }
    }
  }
}
