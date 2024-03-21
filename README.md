#ComposeFree

ComposeFree is a small lib inspired by Runars
[Composable Application Architecture](http://functionaltalks.org/2014/11/23/runar-oli-bjarnason-free-monad/)
which minimizes the boilerplate required to build an application based on a coproduct of
Free DSLs.

To use it, include in build.sbt

```scala
resolvers += Resolver.bintrayRepo("bondlink", "composefree")

libraryDependencies += "bondlink" %% "composefree" % "6.1.1"
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

```scala
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

```scala
import composefree.puredsl._

object RunPure extends (PureOp ~> Id) {
  def apply[A](op: PureOp[A]) = op match {
    case pure(a) => a
  }
}
```

Then we can define the coproduct type for our application, and obtain our ComposeFree
instance.

```scala
import cats.data.EitherK
import composefree.{ComposeFree, ComposeFreeSyntax}

object Program {
  type Program[A] = EitherK[ConsoleOps, PureOp, A]
}

object compose extends ComposeFree[Program.Program] with ComposeFreeSyntax[Program.Program]
```

Last we will create an interpreter for our program type by combining our individual
interpreters.

```scala
import compose._

val interp = RunConsole |: RunPure
```

And finally we can define a program and execute it.

```scala
val prog1: compose.Composed[Unit] =
  for {
    s <- pure("Hello world!").as[PureOp]
    // use of .as[T] helper to cast as super type is
    // required for operations with type parameters that cannot be
    // implicitly converted to the correct coproduct member type
    _ <- print(s)
  } yield ()
// prog1: Free[[_$12 >: Nothing <: Any] => RecNode[Program, _$12], Unit] = FlatMapped(
//   c = Suspend(a = EitherK(run = Right(value = pure(a = "Hello world!")))),
//   f = repl.MdocSession$MdocApp$$Lambda/0x0000007003e00218@7f2657c7
// )

prog1.runWith(interp)
// Hello world!
```

Composite commands can be defined in individual DSLs and mixed into
larger programs as follows.

```scala
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
// prog2: Free[[_$12 >: Nothing <: Any] => RecNode[Program, _$12], Unit] = FlatMapped(
//   c = FlatMapped(
//     c = FlatMapped(
//       c = FlatMapped(
//         c = FlatMapped(
//           c = Suspend(a = EitherK(run = Right(value = pure(a = "Hello")))),
//           f = cats.free.Free$$Lambda/0x0000007003e1e000@12fab5f3
//         ),
//         f = cats.StackSafeMonad$$Lambda/0x0000007003e1f668@4838c135
//       ),
//       f = cats.free.Free$$Lambda/0x0000007003e1e000@3217ad19
//     ),
//     f = cats.StackSafeMonad$$Lambda/0x0000007003e1f668@3c3c656e
//   ),
//   f = repl.MdocSession$MdocApp$$Lambda/0x0000007003e1d0a8@7b2e1a37
// )

prog2.runWith(interp)
// Hello
// World!
```
