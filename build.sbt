name := """composefree"""

version := "1.0.1"

scalaVersion := "2.11.8"

lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.11.8",
  organization := "bondlink",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8", // yes, this is 2 args
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-infer-any",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"),
  publishLocal := {},
  bintrayReleaseOnPublish in ThisBuild := false)

lazy val core = project.in(file("core")).
  settings(commonSettings: _*).
  settings(tutSettings).
  settings(
    name := "composefree",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.4"),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.8.0"),
    tutTargetDirectory := file("."),
    bintrayOrganization := Some("bondlink"),
    bintrayReleaseOnPublish in ThisBuild := false,
    bintrayRepository := "composefree",
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))

lazy val example = project.in(file("example")).
  dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    name := "composefree-example",
    publish := {})
