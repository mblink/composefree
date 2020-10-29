package composefree.example

import cats.~>
import cats.data.EitherK
import cats.effect.IO
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

  object RunPureFuture extends (PureOp ~> Future) {
    def apply[A](p: PureOp[A]) = p match {
      case pure(p) => Future.successful(p)
    }
  }

  object RunPureIO extends (PureOp ~> IO) {
    def apply[A](p: PureOp[A]) = p match {
      case pure(p) => IO.pure(p)
    }
  }

  val futureInterp: Program ~> Future =
    RunConsoleFuture |: RunPureFuture |: RunNumbersFuture()

  val ioInterp: Program ~> IO =
    RunConsoleIO |: RunPureIO |: RunNumbersIO()
}

