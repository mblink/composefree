package composefree.example

import composefree.ComposeFree
import composefree.example.console._
import composefree.example.numbers._
import freek._

object dsl {
  type Program = Console :|: Numbers :|: NilDSL
}

object examplecompose extends ComposeFree(DSL.Make[dsl.Program]) {
  val interp = RunConsole :&: RunNumbers()
}

