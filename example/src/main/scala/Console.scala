package composefree.example

import scalaz.Id.Id
import scalaz.~>

object console {
  sealed trait Console[A]
  case class print(s: String) extends Console[Unit]

  object RunConsole extends (Console ~> Id) {
    def apply[A](c: Console[A]) = c match {
      case print(s) => println(s)
    }
  }
}


