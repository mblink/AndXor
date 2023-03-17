import sbt.*
import sbt.Keys.*
import sbtgitpublish.GitPublishKeys.*

package object andxor {
  val currentVersion = "0.10.0"

  val catsVersion = "2.9.0"
  val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % "test"

  val monocleVersion = "3.2.0"
  val monocleCore = "dev.optics" %% "monocle-core" % monocleVersion
  val monocleLaws = "dev.optics" %% "monocle-law" % monocleVersion % "test"

  val scalacheckVersion = "1.17.0"
  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion

  val scalariform = ("org.scalariform" %% "scalariform" % "0.2.10").cross(CrossVersion.for3Use2_13)

  trait CommonBuild {
    val baseSettings0: Seq[Setting[_]]

    final lazy val baseSettings = Seq(
      organization := "andxor",
      version := currentVersion,
      publish / skip  := true,
      Compile / packageDoc / publishArtifact := false,
      packageDoc / publishArtifact := false,
      Compile / doc / sources := Seq()
    ) ++ baseSettings0

    final lazy val commonSettings = baseSettings ++ Seq(
      libraryDependencies ++= Seq(
        catsCore,
        catsLaws,
        monocleCore,
        monocleLaws,
      )
    )

    final lazy val publishSettings = Seq(
      publish / skip := false,
      gitPublishDir := file("/src/maven-repo"),
      licenses += License.Apache2,
    )

    final lazy val testSettings = Seq(libraryDependencies += scalacheck % "test")

    def generateBase = Project("generate", file("generate"))
      .settings(commonSettings)
      .settings(
        name := "andxor-generate",
        libraryDependencies += scalariform,
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
        libraryDependencies += scalacheck
      )

    def testsBase = Project("tests", file("tests"))
      .settings(commonSettings)
      .settings(name := "andxor-tests")
  }
}