lazy val scala212 = "2.12.10"
lazy val scala213 = "2.13.1"

val scala212_opts = Seq(
  "-Xfuture",
  "-Xlint:by-name-right-associative",
  "-Xlint:unsound-match",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit"
)

val scala212_213_opts = Seq(
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-override",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
  "-Ywarn-unused:locals",
  "-Ywarn-unused:params",
  "-Ywarn-unused:patvars",
  "-Ywarn-unused:privates",
  "-Ywarn-extra-implicit",
  "-Ycache-plugin-class-loader:last-modified",
  "-Ycache-macro-class-loader:last-modified"
)

lazy val commonSettings = Seq(
  version := "3.0.1-LOCAL1",
  organization := "bondlink",
  scalaVersion := scala212,
  crossScalaVersions := Seq(scala212, scala213),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-explaintypes",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xcheckinit",
    "-Xfatal-warnings",
    "-Yrangepos",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => scala212_opts ++ scala212_213_opts
    case Some((2, 13)) => scala212_213_opts
    case _ => Seq()
  }),
  scalacOptions in (Compile, console) --= Seq("-Xlint", "-Xfatal-warnings"),
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
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full,
      "com.projectseptember" %% "freek" % "0.7.0-LOCAL2",
      "io.estatico" %% "newtype" % "0.4.3",
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
  .aggregate(core, example, bench)
  .enablePlugins(TutPlugin)
