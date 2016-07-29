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

Basic use is a paired down version of the manual process, with the following high level steps:

* Define DSLs as sealed families of case classes
* Define interpreters for DSLs as NaturalTransformations
* Define the application Coproduct type for your composed DSLs
* Create an instance of ComposeFree[YourApplicationType] and import it

For example, let's say we wanted to combine a simple Console DSL with the Pure dsl
provided in the ComposeFree lib.

First we define an ADT for our Console operations, and pattern match it
in a NaturalTransformation to an effectful monad.

```scala
scala> import scalaz.Id.Id
import scalaz.Id.Id

scala> import scalaz.~>
import scalaz.$tilde$greater

scala> sealed trait ConsoleOps[A]
defined trait ConsoleOps

scala> case class print(s: String) extends ConsoleOps[Unit]
defined class print

scala> object RunConsole extends (ConsoleOps ~> Id) {
     |   def apply[A](op: ConsoleOps[A]) = op match {
     |     case print(s) => println(s)
     |   }
     | }
defined object RunConsole
```

Now we need a NaturalTransformation for the Pure dsl.

```scala
scala> import composefree.puredsl._
import composefree.puredsl._

scala> object RunPure extends (PureOp ~> Id) {
     |   def apply[A](op: PureOp[A]) = op match {
     |     case Pure(a) => a
     |   }
     | }
defined object RunPure
```

Then we can define the Coproduct type for our application, and obtain our ComposeFree
instance.

```scala
scala> import composefree.ComposeFree
import composefree.ComposeFree

scala> import scalaz.Coproduct
import scalaz.Coproduct

scala> object Program {
     |   type Program[A] = Coproduct[ConsoleOps, PureOp, A]
     | }
defined object Program

scala> object compose extends ComposeFree[Program.Program]
defined object compose
```

Last we will create an interpreter for our program type by combining our individual
interpreters.

```scala
scala> import composefree.syntax._
import composefree.syntax._

scala> val interp = RunConsole.or(RunPure)
interp: composefree.syntax.CNat[ConsoleOps,composefree.puredsl.PureOp,scalaz.Id.Id] = composefree.syntax$CNat@19092ea8
```

And finally we can define a program and execute it.

```scala
scala> import compose._
import compose._

scala> val prog = for {
     |   s <- pure("Hello world!")
     |   _ <- print(s)
     | } yield ()
prog: scalaz.Free[Program.Program,Unit] = Gosub(Suspend(Coproduct(\/-(Pure(Hello world!)))),<function1>)

scala> prog.runWith(interp)
Hello world!
res0: scalaz.Id.Id[Unit] = ()
```
