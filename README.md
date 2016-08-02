#ComposeFree

ComposeFree is a small lib inspired by Runars
[Composable Application Architecture](http://functionaltalks.org/2014/11/23/runar-oli-bjarnason-free-monad/)
which minimizes the boilerplate required to build an application based on a Coproduct of
Free DSLs.

To use it, include in build.sbt

```scala
resolvers += Resolver.bintrayRepo("bondlink", "composefree")

libraryDependencies += "bondlink" %% "composefree" % "0.1.0"
```

Basic use is a pared down version of the manual process, with the following high level steps:

* Define DSLs as sealed families of case classes
* Define interpreters for DSLs as NaturalTransformations
* Define the application Coproduct type for your composed DSLs
* Create an instance of ComposeFree[YourApplicationType] and import it

For example, let's say we wanted to combine a simple Console DSL with the Pure DSL
provided in the ComposeFree lib.

First we define an ADT for our Console operations, and pattern match it
in a NaturalTransformation to an effectful monad.

```scala
import scalaz.Id.Id
import scalaz.~>

sealed trait ConsoleOps[A]
case class print(s: String) extends ConsoleOps[Unit]

object RunConsole extends (ConsoleOps ~> Id) {
  def apply[A](op: ConsoleOps[A]) = op match {
    case print(s) => println(s)
  }
}
```

Now we need a NaturalTransformation for the Pure dsl.

```scala
import composefree.puredsl._

object RunPure extends (PureOp ~> Id) {
  def apply[A](op: PureOp[A]) = op match {
    case Pure(a) => a
  }
}
```

Then we can define the Coproduct type for our application, and obtain our ComposeFree
instance.

```scala
import composefree.ComposeFree
import scalaz.Coproduct

object Program {
  type Program[A] = Coproduct[ConsoleOps, PureOp, A]
}

object compose extends ComposeFree[Program.Program]
```

Last we will create an interpreter for our program type by combining our individual
interpreters.

```scala
import composefree.syntax._

val interp = RunConsole.or(RunPure)
```

And finally we can define a program and execute it.

```scala
import compose._
// import compose._

val prog = for {
  s <- pure("Hello world!")
  _ <- print(s)
} yield ()
// prog: scalaz.Free[Program.Program,Unit] = Gosub(Suspend(Coproduct(\/-(Pure(Hello world!)))),<function1>)

prog.runWith(interp)
// Hello world!
// res6: scalaz.Id.Id[Unit] = ()
```
