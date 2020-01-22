package composefree.bench

import cats.~>
import cats.instances.future._
import composefree._
import composefree.example.Example
import composefree.example.console._
import composefree.example.numbers._
import freek._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import org.openjdk.jmh.annotations.{Benchmark => bench}

object helper {
  val interp = runNumbers :&: Lambda[Console ~> Future](_ match {
    case print(_) => Future.successful(())
  })
}

class Benchmark {
  import helper._

  @bench def test: Int = Await.result(Example(0L).prog.runWith(interp), Duration.Inf)
}
