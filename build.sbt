Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.10"
lazy val scala3 = "3.3.0"

lazy val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

def forScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val commonSettings = Seq(
  version := "6.0.0",
  organization := "bondlink",
  scalaVersion := scala3,
  crossScalaVersions := Seq(scala213, scala3),
  scalacOptions ++= Seq(
    "-Wconf:msg=package object inheritance is deprecated:s",
  ) ++ forScalaV(scalaVersion.value)(
    Seq("-Xsource:3.3", "-Ymacro-annotations"),
    Seq(),
  ),
  libraryDependencies ++= forScalaV(scalaVersion.value)(
    Seq(kindProjector),
    Seq(),
  ),
  publish / skip := true
)

commonSettings
gitRelease := {}

lazy val catsVersion = "2.9.0"
lazy val catsCore = "org.typelevel" %% "cats-core" % catsVersion
lazy val catsFree = "org.typelevel" %% "cats-free" % catsVersion
lazy val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % Test
lazy val newtype = "io.estatico" %% "newtype" % "0.4.4"
lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.17.0" % Test

lazy val publishSettings = Seq(
  publish / skip := false,
  gitPublishDir := file("/src/maven-repo"),
  licenses += License.Apache2,
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
  .settings(commonSettings ++ publishSettings ++ Seq(
    name := "composefree-future",
    libraryDependencies ++= Seq(
      catsCore,
      catsLaws,
      scalaCheck,
    ),
    libraryDependencies ++= forScalaV(scalaVersion.value)(
      Seq(newtype),
      Seq(),
    ),
  ))

lazy val example = project.in(file("example"))
  .settings(commonSettings ++ Seq(
    name := "composefree-example",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.8",
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
