Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.15"
lazy val scala3 = "3.3.7"
lazy val scalaVersions = Seq(scala213, scala3)

ThisBuild / crossScalaVersions := scalaVersions

// GitHub Actions config
val javaVersions = Seq(8, 11, 17, 21).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("master")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"

ThisBuild / githubWorkflowBuild ++= Seq(
  WorkflowStep.Sbt(List("example/run"), name = Some("Run example"), cond = Some(isJava(21))),
  WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Build docs"), cond = Some(isJava(21))),
)

def forScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val commonSettings = Seq(
  version := "7.0.0",
  organization := "bondlink",
  scalaVersion := scala3,
  crossScalaVersions := scalaVersions,
  scalacOptions ++= Seq(
    "-Wconf:msg=package object inheritance is deprecated:s",
  ) ++ forScalaV(scalaVersion.value)(
    Seq("-Xsource:3.3", "-Ymacro-annotations"),
    Seq(),
  ),
  libraryDependencies ++= forScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full)),
    Seq(),
  ),
  publish / skip := true
)

commonSettings

lazy val catsVersion = "2.12.0"
lazy val catsCore = "org.typelevel" %% "cats-core" % catsVersion
lazy val catsFree = "org.typelevel" %% "cats-free" % catsVersion
lazy val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % Test
lazy val newtype = "io.estatico" %% "newtype" % "0.4.4"
lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.18.1" % Test

lazy val publishSettings = Seq(
  publish / skip := false,
  licenses += License.Apache2,
  publishTo := Some("BondLink S3".at("s3://bondlink-maven-repo")),
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
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4",
  ))
  .dependsOn(core, future)
  .aggregate(core, future)

lazy val docs = project.in(file("composefree-docs"))
  .settings(commonSettings ++ Seq(
    mdocOut := file("."),
    scalacOptions ~= (_.filterNot(o => o == "-Xfatal-warnings" || o.startsWith("-Wconf:msg=package"))),
  ))
  .dependsOn(core, future)
  .enablePlugins(MdocPlugin)
