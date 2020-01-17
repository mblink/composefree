package composefree.example

import cats.~>
import composefree.ComposeFree
import composefree.example.console._
import composefree.example.numbers._
import composefree.puredsl._
import freek._
import scala.concurrent.Future

object dsl {
  type Program = Console :|: Numbers :|: PureOp :|: NilDSL
  val Program = DSL.Make[Program]
}

object examplecompose extends ComposeFree[dsl.Program, dsl.Program.Cop] {
  object RunPure extends (PureOp ~> Future) {
    def apply[A](p: PureOp[A]) = p match {
      case pure(p) => Future.successful(p)
    }
  }

  val interp: Interpreter[dsl.Program.Cop, Future] = RunConsole :&: RunNumbers() :&: RunPure
}

