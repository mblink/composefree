name := """composefree"""

scalaVersion in ThisBuild := "2.12.8"
crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.8")

lazy val commonSettings = Seq(
  version := "2.0.0",
  organization := "bondlink",
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
  bintrayReleaseOnPublish in ThisBuild := false
)

lazy val catsVersion = "1.5.0"

lazy val core = project.in(file("core")).
  settings(commonSettings: _*).
  settings(
    name := "composefree",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion
    ),
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value,
    bintrayOrganization := Some("bondlink"),
    bintrayReleaseOnPublish in ThisBuild := false,
    bintrayRepository := "composefree",
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))).
  enablePlugins(TutPlugin)

lazy val example = project.in(file("example")).
  dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    name := "composefree-example",
    publish := {})
