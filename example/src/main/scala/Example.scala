package composefree.example

import cats.instances.future._
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.traverse._
import composefree.example.console._
import composefree.example.dsl._
import composefree.example.numbers._
import composefree.puredsl._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await

object Example {
  import examplecompose._

  def stall[A](a: A): Composed[A] =
    for {
      _ <- pure(println("stalling")).as[PureOp]
      _ <- pure { Thread.sleep(3500L) }.as[PureOp]
      _ = println(a)
    } yield a

  val progA: Composed[Int] = (stall(1).opAp, stall(2).opAp).mapN(_ + _).op

  val prog: Composed[Int] =
    for {
      init <- pure(2).as[PureOp]
      _ <- set(init)
      _ <- update(_ + 1).as[Program].op
      x <- (stall(2).opAp, stall(1).opAp).mapN(_ + _).op
      t <- stall(3)
      a <- add(t)
      _ <- Option("foo").traverse(print(_).op)
      b <- minus(2)
      _ <- print(b.toString)
      c <- add(10)
      r <- get()
    } yield a + b + c + r

  def main(args: Array[String]): Unit = {
    println(Await.result(prog.runWith(examplecompose.interp), 10.seconds))
  }
}
