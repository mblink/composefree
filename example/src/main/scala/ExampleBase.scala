package composefree.example

import cats.Parallel
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.apply._
import cats.syntax.traverse._
import composefree.example.console._
import composefree.example.dsl._
import composefree.example.numbers._
import composefree.future.ParFuture
import composefree.puredsl._
import scala.concurrent.{blocking, ExecutionContext, Future}
import scala.concurrent.duration._

abstract class ExampleBase()(implicit ec: ExecutionContext, p: Parallel.Aux[Future, ParFuture]) extends IOApp {
  import examplecompose._

  final def stall[A](a: A): Composed[A] =
    for {
      _ <- print(s"stalling -- $a")
      _ <- pure { blocking(Thread.sleep(3500L)) }.as[PureOp]
      _ <- print(s"done stalling -- $a")
    } yield a

  final val progA: Composed[Int] = (stall(1).opAp, stall(2).opAp).mapN(_ + _).op

  final val prog: Composed[Int] =
    for {
      init <- pure(2).as[PureOp]
      _ <- set(init)
      _ <- update(_ + 1).as[Program].op
      _ <- (stall(2).opAp, stall(1).opAp).mapN(_ + _).op
      t <- stall(3)
      a <- add(t)
      _ <- Option("foo").traverse(print(_).op)
      b <- minus(2)
      _ <- print(b.toString)
      c <- add(10)
      r <- get()
      res = a + b + c + r
      _ <- print(s"result: $res")
    } yield res

  final def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(println(s"************** Future ****************"))
      _ <- IO.fromFuture(IO(prog.runWith(examplecompose.futureInterp))).timeout(10.seconds)
      _ <- IO(println(s"**************************************"))

      _ <- IO(println(s"************** IO ****************"))
      _ <- prog.runWith(examplecompose.ioInterp).timeout(10.seconds)
      _ <- IO(println(s"**************************************"))
    } yield ExitCode.Success
}
