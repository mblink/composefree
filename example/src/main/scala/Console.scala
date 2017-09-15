package composefree.example

import scala.concurrent.Future
import scalaz.~>

object console {
  sealed trait Console[A]
  case class print(s: String) extends Console[Unit]

  object RunConsole extends (Console ~> Future) {
    def apply[A](c: Console[A]) = c match {
      case print(s) => Future.successful(println(s))
    }
  }
}


