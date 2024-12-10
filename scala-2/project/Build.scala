package andxor

import java.io.File
import play.twirl.sbt.Import.TwirlKeys
import play.twirl.sbt.SbtTwirl
import sbt._
import sbt.Keys._
import sbtgitpublish.GitPublishKeys._

object Build extends CommonBuild {
  val scalaVersions = Seq("2.13.15")

  def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String): Seq[java.io.File] =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) => Seq(srcBaseDir / srcName / "scala-2.13")
      case _ => Seq()
    }

  val baseSettings0 = Seq(
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("2.13")).get,
    scalacOptions ++= Seq(
      "-Vimplicits",
      "-Vimplicits-verbose-tree",
      "-Xlint:strict-unsealed-patmat",
      "-Ymacro-annotations",
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full),
    Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
    Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
  )

  override def coreBase = super.coreBase
    .settings(
      libraryDependencies ++= Seq(
        newtype,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      ),
    )

  override def generateBase = super.generateBase
    .settings(
      resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
      libraryDependencies ++= Seq(
        "com.github.pathikrit" %% "better-files" % "3.9.2",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      ),
      dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
      TwirlKeys.templateImports := Seq(),
      gitRelease := {}
    )
    .enablePlugins(SbtTwirl)

  def argonautBase = Project("argonaut", file("argonaut"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(testSettings)
    .settings(Seq(
      name := "andxor-argonaut",
      libraryDependencies += "io.argonaut" %% "argonaut" % "6.3.10"
    ))

  val circeVersion = "0.14.10"
  def circeBase = Project("circe", file("circe"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(Seq(
      name := "andxor-circe",
      libraryDependencies ++= Seq(
        "io.circe" %% "circe-core" % circeVersion,
        "io.circe" %% "circe-parser" % circeVersion,
      )
    ))
}
