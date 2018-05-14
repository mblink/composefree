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
  type Command[A] = Coproduct[Console, PN, A]

  type ConsolePure[A] = Coproduct[Console, PureOp, A]
}

object examplecompose extends ComposeFree[Command] {

  object RunPure extends (PureOp ~> Future) {
    def apply[A](p: PureOp[A]) = p match {
      case pure(p) => Future.successful(p)
    }
  }

  val interp: Command ~> Future = RunConsole |: RunPure |: RunNumbers()
}

object consolePureCompose extends ComposeFree[ConsolePure]
