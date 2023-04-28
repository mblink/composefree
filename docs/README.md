#ComposeFree

ComposeFree is a small lib inspired by Runars
[Composable Application Architecture](http://functionaltalks.org/2014/11/23/runar-oli-bjarnason-free-monad/)
which minimizes the boilerplate required to build an application based on a coproduct of
Free DSLs.

To use it, include in build.sbt

```scala
resolvers += Resolver.bintrayRepo("bondlink", "composefree")

libraryDependencies += "bondlink" %% "composefree" % "2.0.0"
```

Basic use is a pared down version of the manual process, with the following high level steps:

* Define DSLs as sealed families of case classes
* Define interpreters for DSLs as natural transformations
* Define the application Coproduct type for your composed DSLs
* Create an instance of ComposeFree[YourApplicationType] and import it

For example, let's say we wanted to combine a simple Console DSL with the Pure DSL
provided in the ComposeFree lib.

First we define an ADT for our Console operations, and pattern match it
in a natural transformation to an effectful monad.

```scala mdoc:silent
import cats._

sealed trait ConsoleOps[A]
case class print(s: String) extends ConsoleOps[Unit]

object RunConsole extends (ConsoleOps ~> Id) {
  def apply[A](op: ConsoleOps[A]) = op match {
    case print(s) => println(s)
  }
}
```

Now we need a natural transformation for the Pure dsl.

```scala mdoc:silent
import composefree.puredsl._

object RunPure extends (PureOp ~> Id) {
  def apply[A](op: PureOp[A]) = op match {
    case pure(a) => a
  }
}
```

Then we can define the coproduct type for our application, and obtain our ComposeFree
instance.

```scala mdoc:silent
import cats.data.EitherK
import composefree.{ComposeFree, ComposeFreeSyntax}

object Program {
  type Program[A] = EitherK[ConsoleOps, PureOp, A]
}

object compose extends ComposeFree[Program.Program] with ComposeFreeSyntax[Program.Program]
```

Last we will create an interpreter for our program type by combining our individual
interpreters.

```scala mdoc:silent
import compose._

val interp = RunConsole |: RunPure
```

And finally we can define a program and execute it.

```scala mdoc
val prog1: compose.Composed[Unit] =
  for {
    s <- pure("Hello world!").as[PureOp]
    // use of .as[T] helper to cast as super type is
    // required for operations with type parameters that cannot be
    // implicitly converted to the correct coproduct member type
    _ <- print(s)
  } yield ()

prog1.runWith(interp)
```

Composite commands can be defined in individual DSLs and mixed into
larger programs as follows.

```scala mdoc
object PureComposite {
  import compose.lift._
  import cats.free.Free

  def makeTuple(s1: String, s2: String): Free[PureOp, (String, String)] =
    for {
      a <- pure(s1).as[PureOp].liftF
      b <- pure(s2).as[PureOp].liftF
    } yield (a, b)
}

import Program._

val prog2 = for {
  s <- PureComposite.makeTuple("Hello", "World!").as[Program].op
  _ <- print(s._1)
  _ <- print(s._2)
} yield ()

prog2.runWith(interp)
```
