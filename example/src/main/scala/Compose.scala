package composefree.example

import composefree.ComposeFree
import composefree.puredsl._
import composefree.example.console._
import composefree.example.dsl._
import composefree.example.numbers._
import scalaz.{Coproduct, Free, Trampoline, ~>}
import scalaz.Trampoline._

object dsl {
  type PN[A] = Coproduct[PureOp, Numbers, A]
  type Program[A] = Coproduct[Console, PN, A]
}

object examplecompose extends ComposeFree[Program] {

  object RunPure extends (PureOp ~> Free.Trampoline) {
    def apply[A](p: PureOp[A]) = p match {
      case Pure(p) => delay(p)
    }
  }

  val interp: Program ~> Free.Trampoline =
    RunConsole.or(RunPure.or(RunNumbers()): (PN ~> Free.Trampoline))
}

