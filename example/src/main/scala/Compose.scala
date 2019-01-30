package composefree.example

import cats.~>
import cats.data.EitherK
import composefree.ComposeFree
import composefree.puredsl._
import composefree.example.console._
import composefree.example.dsl._
import composefree.example.numbers._
import scala.concurrent.Future

object dsl {
  type PN[A] = EitherK[PureOp, Numbers, A]
  type Program[A] = EitherK[Console, PN, A]
}

object examplecompose extends ComposeFree[Program] {

  object RunPure extends (PureOp ~> Future) {
    def apply[A](p: PureOp[A]) = p match {
      case pure(p) => Future.successful(p)
    }
  }

  val interp: Program ~> Future =
    RunConsole.or(RunPure.or(RunNumbers()): (PN ~> Future))
}

