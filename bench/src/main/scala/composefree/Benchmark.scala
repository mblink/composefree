package composefree.bench

import cats.~>
import composefree.example.Example
import composefree.example.examplecompose._
import composefree.example.console._
import composefree.example.numbers._
import freek._
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import org.openjdk.jmh.annotations.{Benchmark => bench}

object helper {
  val interp = RunPure :&: RunNumbers() :&: Lambda[Console ~> Future](_ match {
    case print(_) => Future.successful(())
  })
}

class Benchmark {
  import helper._

  @bench def test: Int = Await.result(Example(0L).runWith(interp), Duration.Inf)
}
