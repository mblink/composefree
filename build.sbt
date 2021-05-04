Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala212 = "2.12.13"
lazy val scala213 = "2.13.5"

def forScalaV[A](scalaVersion: String)(_213: => A, _212: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((2, 12)) => _212
  }

lazy val commonSettings = Seq(
  version := "4.1.1",
  organization := "bondlink",
  scalaVersion := scala213,
  crossScalaVersions := Seq(scala212, scala213),
  Compile / console / scalacOptions ~= filterConsoleScalacOptions,
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full),
  publish / skip := true
)

commonSettings
gitRelease := {}

lazy val catsVersion = "2.6.0"
lazy val catsCore = "org.typelevel" %% "cats-core" % catsVersion
lazy val catsFree = "org.typelevel" %% "cats-free" % catsVersion
lazy val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % Test
lazy val newtype = "io.estatico" %% "newtype" % "0.4.4"
lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.3" % Test

lazy val publishSettings = Seq(
  publish / skip := false,
  gitPublishDir := file("/src/maven-repo"),
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val macroAnnotationSettings =
  Seq(
    scalacOptions ++= forScalaV(scalaVersion.value)(Seq("-Ymacro-annotations"), Seq()),
    libraryDependencies ++= forScalaV(scalaVersion.value)(Seq(),
      Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)))
  )

lazy val core = project.in(file("core"))
  .settings(commonSettings ++ publishSettings ++ Seq(
    name := "composefree",
    libraryDependencies ++= Seq(
      catsCore,
      catsFree
    )
  ))

lazy val future = project.in(file("future"))
  .settings(commonSettings ++ publishSettings ++ macroAnnotationSettings ++ Seq(
    name := "composefree-future",
    libraryDependencies ++= Seq(
      catsCore,
      catsLaws,
      newtype,
      scalaCheck
    )
  ))

lazy val example = project.in(file("example"))
  .settings(commonSettings ++ Seq(
    name := "composefree-example",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.0",
    gitRelease := {}
  ))
  .dependsOn(core, future)
  .aggregate(core, future)

lazy val docs = project.in(file("composefree-docs"))
  .settings(commonSettings ++ Seq(
    mdocOut := file("."),
    scalacOptions -= "-Xfatal-warnings",
    gitRelease := {}
  ))
  .dependsOn(core, future)
  .enablePlugins(MdocPlugin)
