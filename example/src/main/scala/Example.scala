package composefree.example

import cats.~>
import cats.instances.future._
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.traverse._
import composefree._
import composefree.example.console._
import composefree.example.numbers._
import composefree.puredsl._
import freek._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class Example(sleep: Long) {
  def stall[A](a: A) =
    composefree.test.using[Console :|: PureOp :|: NilDSL].dsl(implicit c =>
      for {
        _ <- print(s"stalling: $a").as[Console].op
        _ <- pure(if (sleep == 0L) () else Thread.sleep(sleep)).as[PureOp].op
        _ <- print(a.toString).as[Console].op
      } yield a)

  import examplecompose._

  def progA: Composed[dsl.Program, Int] =
    composefree.test.using[Console :|: PureOp :|: NilDSL].dsl(implicit c =>
      (stall(1).opAp, stall(2).opAp).mapN(_ + _).op)

  def prog: Composed[dsl.Program, Int] =
    composefree.test.using[dsl.Program].dsl(implicit c =>
      for {
        init <- pure(2).as[PureOp].op
        _ <- set(init)
        _ <- update(_ + 1).op
        _ <- (stall(2).opAp, stall(1).opAp).mapN(_ + _).op
        t <- stall(3)
        a <- add(t)
        _ <- Option("foo").traverse(print(_).op)
        b <- minus(2)
        _ <- print(b.toString)
        c <- add(10)
        r <- get()
      } yield a + b + c + r)


  def runWith(i: dsl.Program.Cop ~> Future): Future[Int] =
    prog.runWith(i)

  def runWith[F[_] <: CopK[_]](i: Interpreter[F, Future])(implicit s: SubCop[dsl.Program.Cop, F]): Future[Int] =
    runWith(Lambda[dsl.Program.Cop ~> Future](p => i.nat(s(p))))

  def run: Future[Int] =
    runWith(interp)
}

object Example {
  def main(args: Array[String]): Unit =
    println(Await.result(Example(3500L).run, 10.seconds))
}
