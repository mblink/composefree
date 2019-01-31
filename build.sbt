lazy val scala212 = "2.12.8"
lazy val scala211 = "2.11.12"

lazy val commonSettings = Seq(
  version := "2.0.0",
  organization := "bondlink",
  scalaVersion := scala212,
  crossScalaVersions := Seq(scala211, scala212),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8", // yes, this is 2 args
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Ypartial-unification",
    "-Yno-adapted-args",
    "-Ywarn-infer-any",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  ),
  scalacOptions in (Compile, console) --= Seq("-Xlint", "-Xfatal-warnings"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
  bintrayReleaseOnPublish in ThisBuild := false,
  skip in publish := true
)

lazy val catsVersion = "1.5.0"

lazy val publishSettings = Seq(
  skip in publish := false,
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "composefree",
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val core = project.in(file("core"))
  .settings(commonSettings ++ publishSettings ++ Seq(
    name := "composefree",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion
    )
  ))

lazy val example = project.in(file("example"))
  .dependsOn(core)
  .settings(commonSettings ++ Seq(
    name := "composefree-example",
    bintrayRelease := {}
  ))

lazy val root = project.in(file("."))
  .settings(commonSettings ++ Seq(
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value,
    bintrayRelease := {}
  ))
  .dependsOn(core)
  .aggregate(core, example)
  .enablePlugins(TutPlugin)
