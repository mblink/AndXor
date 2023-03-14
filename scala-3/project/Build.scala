package andxor

import java.io.File
import sbt.*
import sbt.Keys.*
import sbtgitpublish.GitPublishKeys.*

object Build {
  lazy val scalaVersions = Seq("3.3.0-RC3")

  val lintOpts = Seq(
    "-Wvalue-discard",
    "-Wunused:implicits",
    "-Wunused:imports",
    "-Wunused:locals",
    "-Wunused:params",
    "-Wunused:privates",
  )

  val baseSettings = Seq(
    organization := "andxor",
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("3.")).get,
    scalacOptions ++= lintOpts ++ Seq(
      "-explain",
      "-Yexplain-lowlevel",
    ),
    version := currentVersion,
    publish / skip  := true,
    Compile / packageDoc / publishArtifact := false,
    packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq()
  )

  val catsVersion = "2.9.0"
  val monocleVersion = "3.2.0"
  val scalacheckVersion = "1.17.0"
  val scalacheckDep = "org.scalacheck" %% "scalacheck" % scalacheckVersion

  val commonSettings = baseSettings ++ Seq(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-laws" % catsVersion % "test",
      "dev.optics" %% "monocle-core" % monocleVersion,
      "dev.optics" %% "monocle-law" % monocleVersion % "test"
    )
  )

  val publishSettings = Seq(
    publish / skip := false,
    gitPublishDir := file("/src/maven-repo"),
    licenses += License.Apache2,
  )

  val testSettings = Seq(libraryDependencies += scalacheckDep % "test")

  def generateBase = Project("generate", file("generate"))
    .settings(commonSettings)
    .settings(
      name := "andxor-generate",
      libraryDependencies ++= Seq(
        ("org.scalariform" %% "scalariform" % "0.2.10").cross(CrossVersion.for3Use2_13),
      ),
      gitRelease := {}
    )

  def coreBase = Project("core", file("core"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(testSettings)
    .settings(name := "andxor-core")

  def scalacheckBase = Project("scalacheck", file("scalacheck"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(
      name := "andxor-scalacheck",
      libraryDependencies += scalacheckDep
    )

  def testsBase = Project("tests", file("tests"))
    .settings(commonSettings)
    .settings(
      name := "andxor-tests",
      scalacOptions --= lintOpts,
    )
}
