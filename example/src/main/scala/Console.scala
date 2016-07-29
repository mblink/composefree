package composefree.example

import scalaz.Trampoline._
import scalaz.{Free, Trampoline, ~>}

object console {
  sealed trait Console[A]
  case class print(s: String) extends Console[Unit]

  object RunConsole extends (Console ~> Free.Trampoline) {
    def apply[A](c: Console[A]) = c match {
      case print(s) => delay(println(s))
    }
  }
}


