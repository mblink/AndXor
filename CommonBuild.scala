import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import sbt.*
import sbt.Keys.*
import sbtghactions.{JavaSpec, WorkflowStep}
import sbtghactions.GenerativeKeys.*
import sbtgitpublish.GitPublishKeys.*

package object andxor {
  val currentVersion = "0.14.0"

  val scala2 = "2.13.13"
  val scala3 = "3.3.3"

  val catsVersion = "2.10.0"
  val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % "test"

  val monocleVersion = "3.2.0"
  val monocleCore = "dev.optics" %% "monocle-core" % monocleVersion
  val monocleLaws = "dev.optics" %% "monocle-law" % monocleVersion % "test"

  val newtype = "io.estatico" %% "newtype" % "0.4.4"

  val scalacheckVersion = "1.17.0"
  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion

  val scalariform = ("org.scalariform" %% "scalariform" % "0.2.10").cross(CrossVersion.for3Use2_13)

  val javaVersions = Seq(8, 11, 17, 21).map(v => JavaSpec.temurin(v.toString))
  val isJava8 = s"matrix.java == '${javaVersions.find(_.version == "8").get.render}'"

  trait CommonBuild {
    val relDir: String
    val baseSettings0: Seq[Setting[_]]

    final lazy val baseSettings = Seq(
      organization := "andxor",
      version := currentVersion,
      publish / skip  := true,
      Compile / packageDoc / publishArtifact := false,
      packageDoc / publishArtifact := false,
      Compile / doc / sources := Seq(),
      mimaPreviousArtifacts := Set(),
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
      resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main",
      mimaPreviousArtifacts := Set("andxor" %% name.value % "0.14.0"),
    )

    final lazy val githubActionsSettings = Seq(
      ThisBuild / githubWorkflowScalaVersions := Some(crossScalaVersions.value).filter(_.nonEmpty).getOrElse(Seq(scalaVersion.value)),
      ThisBuild / githubWorkflowJavaVersions := javaVersions,
      ThisBuild / githubWorkflowArtifactUpload := false,
      ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false),
      ThisBuild / githubWorkflowTargetBranches := Seq("master"),
      ThisBuild / githubWorkflowPublishTargetBranches := Seq(),
      ThisBuild / githubWorkflowSbtCommand := s"cd $relDir && sbt",
      ThisBuild / githubWorkflowBuild ++= Seq(
        WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility"), cond = Some(isJava8)),
        WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Build docs"), cond = Some(isJava8)),
      ),
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
      .settings(
        name := "andxor-tests",
        gitRelease := {},
      )
  }
}
