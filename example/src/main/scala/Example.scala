package composefree.example

import composefree.example.console._
import composefree.example.numbers._
import composefree.puredsl._
import composefree.syntax._

object Example {

  val prog = {
    import examplecompose._
    for {
      init <- pure(2).as[PureOp]
      _ <- set(init)
      a <- add(3)
      b <- minus(2)
      _ <- print(b.toString)
      c <- add(10)
      r <- get()
    } yield a + b + c + r
  }

  def main(args: Array[String]): Unit = {
    println(prog.runWith(examplecompose.interp).run)
  }
}
