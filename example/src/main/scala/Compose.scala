package composefree.example

import composefree.example.console._
import composefree.example.numbers._
import freek._

object examplecompose {
  val interp = runConsole :&: runNumbers
}

