name := """composefree"""

scalaVersion in ThisBuild := "2.12.4"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.4")

lazy val commonSettings = Seq(
  version := "1.1.1",
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
    "-Xfuture",
    "-P:splain:all"),
  bintrayReleaseOnPublish in ThisBuild := false,
  addCompilerPlugin("io.tryp" % "splain" % "0.2.9" cross CrossVersion.patch))

lazy val core = project.in(file("core")).
  settings(commonSettings: _*).
  settings(
    name := "composefree",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
    tutTargetDirectory := file("."),
    bintrayOrganization := Some("bondlink"),
    bintrayReleaseOnPublish in ThisBuild := false,
    bintrayRepository := "composefree",
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))).
  enablePlugins(TutPlugin).
  dependsOn(scalazCore)

lazy val scalazCore = ProjectRef(uri("git://github.com/mrdziuban/scalaz.git#expose-inject-7.2.22"), "coreJVM")

lazy val example = project.in(file("example")).
  dependsOn(core)
  .settings(commonSettings: _*)
  .settings(
    name := "composefree-example",
    publish := {})
