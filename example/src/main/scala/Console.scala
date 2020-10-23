package composefree.example

import cats.~>
import cats.effect.IO
import java.time.ZonedDateTime
import scala.concurrent.Future

object console {
  sealed trait Console[A]
  case class print(s: String) extends Console[Unit]

  def log(s: String): Unit =
    println(s"[${ZonedDateTime.now}] $s")

  object RunConsoleFuture extends (Console ~> Future) {
    def apply[A](c: Console[A]) = c match {
      case print(s) => Future.successful(log(s))
    }
  }

  object RunConsoleIO extends (Console ~> IO) {
    def apply[A](c: Console[A]) = c match {
      case print(s) => IO.pure(log(s))
    }
  }
}


