lazy val scala212 = "2.12.11"
lazy val scala213 = "2.13.3"

lazy val commonSettings = Seq(
  version := "4.0.0-RC1",
  organization := "bondlink",
  scalaVersion := scala212,
  crossScalaVersions := Seq(scala212, scala213),
  scalacOptions in (Compile, console) ~= filterConsoleScalacOptions,
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  bintrayReleaseOnPublish in ThisBuild := false,
  skip in publish := true
)

lazy val catsVersion = "2.2.0"

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
    libraryDependencies += "org.typelevel" %% "cats-effect" % catsVersion,
    bintrayRelease := {}
  ))

lazy val docs = project.in(file("composefree-docs"))
  .settings(
    mdocOut := file(".")
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
