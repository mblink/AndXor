package andxor

import java.io.File
import play.twirl.sbt.Import.TwirlKeys
import play.twirl.sbt.SbtTwirl
import sbt._
import sbt.Keys._
import sbtgitpublish.GitPublishKeys._

object Build {
  lazy val scalaVersions = Seq("2.13.7")

  def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String): Seq[java.io.File] =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 13)) => Seq(srcBaseDir / srcName / "scala-2.13")
      case _ => Seq()
    }

  val baseSettings = Seq(
    organization := "andxor",
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("2.13")).get,
    scalacOptions ++= Seq(
      "-Vimplicits",
      "-Vimplicits-verbose-tree",
      "-Xlint:strict-unsealed-patmat",
    ),
    version := currentVersion,
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
    Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
    Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
    publish / skip  := true,
    Compile / packageDoc / publishArtifact := false,
    packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq()
  )

  val catsVersion = "2.6.1"
  val monocleVersion = "2.1.0"
  val scalacheckVersion = "1.15.4"
  val scalacheckDep = "org.scalacheck" %% "scalacheck" % scalacheckVersion

  val commonSettings = baseSettings ++ Seq(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-laws" % catsVersion % "test",
      "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-law" % monocleVersion % "test"
    )
  )

  val publishSettings = Seq(
    publish / skip := false,
    gitPublishDir := file("/src/maven-repo"),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
  )

  val testSettings = Seq(libraryDependencies += scalacheckDep % "test")

  def generateBase = Project("generate", file("generate"))
    .settings(commonSettings)
    .settings(Seq(
      name := "andxor-generate",
      resolvers += Resolver.sonatypeRepo("snapshots"),
      libraryDependencies ++= Seq(
        "com.github.pathikrit" %% "better-files" % "3.9.1",
        "org.scalariform" %% "scalariform" % "0.2.10",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      ),
      TwirlKeys.templateImports := Seq(),
      gitRelease := {}
    )).enablePlugins(SbtTwirl)

  def coreBase = Project("core", file("core"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(testSettings)
    .settings(Seq(name := "andxor-core"))

  def argonautBase = Project("argonaut", file("argonaut"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(testSettings)
    .settings(Seq(
      name := "andxor-argonaut",
      libraryDependencies += "io.argonaut" %% "argonaut" % "6.3.3"
    ))

  val circeVersion = "0.14.0-M6"
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

  def scalacheckBase = Project("scalacheck", file("scalacheck"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(Seq(
      name := "andxor-scalacheck",
      libraryDependencies += scalacheckDep
    ))

  def enablePlugin(jar: File, extra: Seq[String]): Seq[String] =
    Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}") ++ extra

  def pluginOptions(pluginOpts: Seq[String]) = Seq(
    scalacOptions -= "-Ywarn-unused:patvars",
    Test / scalacOptions ++= enablePlugin((Compile / Keys.`package`).value, pluginOpts),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  )

  def compilerPlugin(proj: Project, nme: String, pluginOpts: Seq[String]) =
    proj
      .settings(baseSettings)
      .settings(publishSettings)
      .settings(pluginOptions(pluginOpts))
      .settings(name := nme)

  def annotationPlugin(proj: Project, nme: String, pluginOpts: Seq[String]) =
    compilerPlugin(proj, nme, pluginOpts)
      .settings(Compile / sourceGenerators += Def.task {
        Seq(baseDirectory.value / ".." / "src" / "files" / "AnnotationPlugin.scala")
      })

  val derivingFlags = Seq(
    "-P:deriving:covariant:Arbitrary",
    "-P:deriving:labelledCovariant:Decoder|DecodeJson|Read",
    "-P:deriving:contravariant:Prod:Csv|Eq",
    "-P:deriving:labelledContravariant:Cop:Csv|Eq",
    "-P:deriving:labelledContravariant:Encoder|EncodeJson|Show"
  )

  def derivingBase = annotationPlugin(Project("deriving", file("deriving")), "andxor-deriving", derivingFlags).settings(testSettings)

  def newtypeBase = annotationPlugin(Project("newtype", file("newtype")), "andxor-newtype", Seq())
}
