package composefree.example

import composefree.example.console._
import composefree.example.dsl._
import composefree.example.numbers._
import composefree.puredsl._
import composefree.syntax._
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.traverse._

object Example {
  import examplecompose._

  val prog =
    for {
      init <- pure(2).as[PureOp].apM
      _ <- set(init).apM
      _ <- update(_ + 1).as[Program].op
      x <- (pure(2).as[PureOp].opAp |@| pure(1).as[PureOp].opAp)(_ + _)
      a <- add(3)
      _ <- Option("foo").traverseU(print(_).op)
      b <- minus(2)
      _ <- print(b.toString)
      c <- add(10)
      r <- get()
    } yield a + b + c + r

  def main(args: Array[String]): Unit = {
    println(prog.runWith(examplecompose.interp).run)
  }
}
