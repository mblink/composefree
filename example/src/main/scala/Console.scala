package composefree.example

import cats.~>
import scala.concurrent.Future

object console {
  sealed trait Console[A]
  case class print(s: String) extends Console[Unit]

  val runConsole = Lambda[Console ~> Future](_ match {
    case print(s) => Future.successful(println(s))
  })
}
