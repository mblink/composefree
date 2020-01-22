package composefree.example

import cats.instances.future._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import composefree._
import composefree.example.console._
import composefree.example.numbers._
import examplecompose.interp
import freek.:|:
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class Example(sleep: Long) {
  def stall[A](a: A): Composed[Console, A] =
    for {
      _ <- print(s"stalling: $a").as[Console]
      _ = if (sleep == 0L) () else Thread.sleep(sleep)
      _ <- print(a.toString).as[Console]
    } yield a

  val parProg: ComposedAp[Console, Int] = (stall(1).opAp, stall(2).opAp).mapN(_ + _)

  def prog: Composed[Console :|: Numbers, Int] =
    for {
      init <- 2.pure[Composed]
      _ <- set(init)
      _ <- update(_ + 1).op
      _ <- parProg.op
      t <- stall(3).op
      a <- add(t)
      _ <- Option("foo").traverse(print(_).op)
      b <- minus(2)
      _ <- print(b.toString)
      c <- add(10)
      r <- get()
    } yield a + b + c + r

  def run: Future[Int] = prog.runWith(interp)
}

object Example {
  def main(args: Array[String]): Unit =
    println(Await.result(Example(3500L).run, 10.seconds))
}
