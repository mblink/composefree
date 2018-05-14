package composefree.example

import composefree.example.consolePureCompose.{Composed => ConsolePureComposed, _}
import scala.concurrent.Future
import scalaz.~>

object console {
  sealed trait Console[A]
  case class print(s: String) extends Console[Unit]

  def printOnlyProg(): ConsolePureComposed[Unit] =
    for {
      _ <- print("foo")
      _ <- print("bar")
    } yield ()

  object RunConsole extends (Console ~> Future) {
    def apply[A](c: Console[A]) = c match {
      case print(s) => Future.successful(println(s))
    }
  }
}


