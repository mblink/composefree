package composefree.example

import composefree.{ComposeFreeNoRecursion, ShapelessComposeFree}
import composefree.puredsl._
import composefree.example.console._
import composefree.example.dsl._
import composefree.example.numbers._
import scala.concurrent.Future
import scalaz.~>
import shapeless.{:+:, CNil}

object dsl {
  type ConsoleProg[A] = Console[A] :+: CNil
  type Program[A] = Console[A] :+: PureOp[A] :+: Numbers[A] :+: CNil
}

object examplecompose extends ShapelessComposeFree[Program] {

  object RunPure extends (PureOp ~> Future) {
    def apply[A](p: PureOp[A]) = p match {
      case pure(p) => Future.successful(p)
    }
  }

  val interp: Program ~> Future = RunConsole |: RunPure |: RunNumbers
}

object consoleOnlyCompose extends ComposeFreeNoRecursion[ConsoleProg] {
  val interp: ConsoleProg ~> Future = RunConsole |: CNilF
}
