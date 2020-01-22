Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala212 = "2.12.10"
lazy val scala213 = "2.13.1"

def foldV[A](version: String)(twlv: => A, thrtn: => A): A = (CrossVersion.partialVersion(version) match {
  case Some((2, 12)) => twlv
  case Some((2, 13)) => thrtn
})

lazy val commonSettings = Seq(
  version := "3.0.1-LOCAL1",
  organization := "bondlink",
  scalaVersion := scala212,
  crossScalaVersions := Seq(scala212, scala213),
  scalacOptions ++= foldV(scalaVersion.value)(Seq(), Seq("-Ymacro-annotations")),
  libraryDependencies ++= foldV(scalaVersion.value)(
    Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)), Seq()),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  bintrayReleaseOnPublish in ThisBuild := false,
  skip in publish := true
)

lazy val catsVersion = "2.1.0"
lazy val silencerVersion = "1.4.4"

lazy val publishSettings = Seq(
  skip in publish := false,
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "composefree",
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val core = project.in(file("core"))
  .settings(commonSettings ++ publishSettings ++ Seq(
    name := "composefree",
    addCompilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full,
      "com.projectseptember" %% "freek" % "0.7.0-LOCAL3",
      "io.estatico" %% "newtype" % "0.4.3",
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion
    )
  ))

lazy val plugin = project.in(file("plugin"))
  .settings(commonSettings ++ publishSettings ++ Seq(
    name := "composefree-plugin",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    scalacOptions -= "-Ywarn-unused:patvars"
  ))

def enablePlugin(jar: File): Seq[String] =
  Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")

lazy val example = project.in(file("example"))
  .dependsOn(core)
  .settings(commonSettings ++ Seq(
    name := "composefree-example",
    bintrayRelease := {},
    scalacOptions ++= enablePlugin((plugin / Compile / Keys.`package`).value),
  ))

lazy val bench = project.in(file("bench"))
  .dependsOn(core, example)
  .settings(commonSettings ++ Seq(
    name := "composefree-bench",
    bintrayRelease := {}
  ))
  .enablePlugins(JmhPlugin)

lazy val root = project.in(file("."))
  .settings(commonSettings ++ Seq(
    crossScalaVersions := Seq(scala212),
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value,
    bintrayRelease := {}
  ))
  .dependsOn(core)
  .aggregate(core, plugin, example, bench)
  .enablePlugins(TutPlugin)
