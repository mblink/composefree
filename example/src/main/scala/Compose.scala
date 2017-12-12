package composefree.example

import composefree.ComposeFree
import composefree.puredsl._
import composefree.example.console._
import composefree.example.dsl._
import composefree.example.numbers._
import scala.concurrent.Future
import scalaz.{Coproduct, ~>}

object dsl {
  type PN[A] = Coproduct[PureOp, Numbers, A]
  type Program[A] = Coproduct[Console, PN, A]
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

