Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala212 = "2.12.12"
lazy val scala213 = "2.13.3"

def forScalaV[A](scalaVersion: String)(_213: => A, _212: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((2, 12)) => _212
  }

lazy val commonSettings = Seq(
  version := "4.0.0-RC1",
  organization := "bondlink",
  scalaVersion := scala213,
  crossScalaVersions := Seq(scala212, scala213),
  scalacOptions in (Compile, console) ~= filterConsoleScalacOptions,
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  bintrayReleaseOnPublish in ThisBuild := false,
  skip in publish := true
)

lazy val catsVersion = "2.2.0"
lazy val catsCore = "org.typelevel" %% "cats-core" % catsVersion
lazy val catsFree = "org.typelevel" %% "cats-free" % catsVersion
lazy val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % Test
lazy val newtype = "io.estatico" %% "newtype" % "0.4.4"
lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.3" % Test

lazy val publishSettings = Seq(
  skip in publish := false,
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "composefree",
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
      catsFree,
      "org.typelevel" %% "cats-free" % catsVersion
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
  .dependsOn(core, future)
  .aggregate(core, future)
  .settings(commonSettings ++ Seq(
    name := "composefree-example",
    libraryDependencies += "org.typelevel" %% "cats-effect" % catsVersion,
    bintrayRelease := {}
  ))

lazy val docs = project.in(file("composefree-docs"))
  .settings(
    mdocOut := file(".")
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
