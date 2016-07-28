package composefree.example

import composefree.ComposeFree
import composefree.puredsl._
import composefree.example.console._
import composefree.example.dsl._
import composefree.example.numbers._
import scalaz.{Coproduct, ~>}
import scalaz.Id.Id

object dsl {
  type PN[A] = Coproduct[PureOp, Numbers, A]
  type Program[A] = Coproduct[Console, PN, A]
}

object examplecompose extends ComposeFree[Program] {

  object RunPure extends (PureOp ~> Id) {
    def apply[A](p: PureOp[A]) = p match {
      case Pure(p) => p
    }
  }

  val interp: Program ~> Id =
    (RunConsole.or(RunPure.or(RunNumbers()): (PN ~> Id)): (Program ~> Id))
}

