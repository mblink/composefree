Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.18"
lazy val scala3 = "3.3.7"

ThisBuild / scalaVersion := scala3

// GitHub Actions config
val javaVersions = Seq(8, 11, 17, 21, 25).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("master")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(List("sbt test"), name = Some("Build project")),
  WorkflowStep.Run(List("sbt example/run"), name = Some("Run example"), cond = Some(isJava(25))),
  WorkflowStep.Run(List("sbt mdoc"), name = Some("Build docs"), cond = Some(isJava(25))),
)

def forScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val commonSettings = Seq(
  version := "7.0.0",
  organization := "bondlink",
  scalacOptions ++= Seq(
    "-Wconf:msg=package object inheritance is deprecated:s",
  ) ++ forScalaV(scalaVersion.value)(
    Seq("-Xsource:3.3", "-Ymacro-annotations"),
    Seq(),
  ),
  libraryDependencies ++= forScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.full)),
    Seq(),
  ),
  publish / skip := true
)

commonSettings

def baseProj(id: String, nme: String) =
  sbt.internal.ProjectMatrix(id, file(id))
    .jvmPlatform(scalaVersions = Seq(scala213, scala3))
    .settings(commonSettings ++ Seq(name := nme))

lazy val catsVersion = "2.13.0"
lazy val catsCore = "org.typelevel" %% "cats-core" % catsVersion
lazy val catsFree = "org.typelevel" %% "cats-free" % catsVersion
lazy val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % Test
lazy val newtype = "io.estatico" %% "newtype" % "0.4.4"
lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.19.0" % Test

lazy val publishSettings = Seq(
  publish / skip := false,
  licenses += License.Apache2,
  publishTo := Some("BondLink S3".at("s3://bondlink-maven-repo")),
)

lazy val core = baseProj("core", "composefree")
  .settings(publishSettings ++ Seq(
    libraryDependencies ++= Seq(
      catsCore,
      catsFree
    )
  ))

lazy val future = baseProj("future", "composefree-future")
  .settings(publishSettings ++ Seq(
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

lazy val example = baseProj("example", "composefree-example")
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.7.0")
  .dependsOn(core, future)
  .aggregate(core, future)

lazy val docs = projectMatrix.in(file("composefree-docs"))
  .jvmPlatform(scalaVersions = Seq(scala3))
  .settings(commonSettings ++ Seq(
    mdocOut := file("."),
    scalacOptions ~= (_.filterNot(o => o == "-Werror" || o.startsWith("-Wconf:msg=package"))),
  ))
  .dependsOn(core, future)
  .enablePlugins(MdocPlugin)
