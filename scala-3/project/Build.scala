package andxor

import java.io.File
import sbt.*
import sbt.Keys.*
import sbtgitpublish.GitPublishKeys.*

object Build {
  lazy val scalaVersions = Seq("3.3.0-RC3")

  def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) => _213
      case Some((3, _)) => _3
    }

  def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String): Seq[java.io.File] =
    foldScalaV(scalaVersion)(
      Seq(srcBaseDir / srcName / "scala-2"),
      Seq(srcBaseDir / srcName / "scala-3"))

  val baseSettings = Seq(
    organization := "andxor",
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("3.")).get,
    scalacOptions ++= foldScalaV(scalaVersion.value)(
      Seq(
        "-Vimplicits-verbose-tree",
        "-Xlint:strict-unsealed-patmat",
      ),
      Seq(
        "-explain",
        "-Yexplain-lowlevel",
      )
    ),
    version := currentVersion,
    libraryDependencies ++= foldScalaV(scalaVersion.value)(
      Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)),
      Seq(),
    ),
    Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
    Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
    publish / skip  := true,
    Compile / packageDoc / publishArtifact := false,
    packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq()
  )

  val catsVersion = "2.9.0"
  val monocleVersion = "3.1.0"
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
    .settings(Seq(
      name := "andxor-generate",
      libraryDependencies ++= Seq(
        ("org.scalariform" %% "scalariform" % "0.2.10").cross(CrossVersion.for3Use2_13),
      ),
      gitRelease := {}
    ))

  def coreBase = Project("core", file("core"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(testSettings)
    .settings(Seq(name := "andxor-core"))

  def scalacheckBase = Project("scalacheck", file("scalacheck"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(Seq(
      name := "andxor-scalacheck",
      libraryDependencies += scalacheckDep
    ))

  def testsBase = Project("tests", file("tests"))
    .settings(commonSettings)
    .settings(Seq(name := "andxor-tests"))
}
