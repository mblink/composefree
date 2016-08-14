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

```tut:book:silent
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

```tut:book:silent
import composefree.puredsl._

object RunPure extends (PureOp ~> Id) {
  def apply[A](op: PureOp[A]) = op match {
    case pure(a) => a
  }
}
```

Then we can define the Coproduct type for our application, and obtain our ComposeFree
instance.

```tut:book:silent
import composefree.ComposeFree
import scalaz.Coproduct

object Program {
  type Program[A] = Coproduct[ConsoleOps, PureOp, A]
}

object compose extends ComposeFree[Program.Program]
```

Last we will create an interpreter for our program type by combining our individual
interpreters.

```tut:book:silent
import composefree.syntax._

val interp = RunConsole.or(RunPure)
```

And finally we can define a program and execute it.

```tut:book
val prog = {
  import compose._
  for {
    s <- pure("Hello world!").as[PureOp]
    // use of .as[T] helper to cast as super type is
    // required for operations with type parameters that cannot be
    // implicitly converted to the correct coproduct member type
    _ <- print(s)
  } yield ()
}

prog.runWith(interp)
```

Composite commands can be defined in individual DSLs and mixed into
larger programs as follows.

```tut:book

object PureComposite {
  import composefree.syntax._
  import composefree.syntax.lift._
  import scalaz.Free

  def makeTuple(s1: String, s2: String): Free[PureOp, (String, String)] =
    for {
      a <- pure(s1).as[PureOp]
      b <- pure(s2).as[PureOp]
    } yield (a, b)
}

import compose._
import Program._

val prog = for {
  s <- PureComposite.makeTuple("Hello", "World!").as[Program]
  _ <- print(s._1)
  _ <- print(s._2)
} yield ()

prog.runWith(interp)
```
