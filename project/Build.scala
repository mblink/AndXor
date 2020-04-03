package andxor

import bintray.BintrayKeys._
import java.io.File
import play.twirl.sbt.Import.TwirlKeys
import play.twirl.sbt.SbtTwirl
import sbt._
import sbt.Keys._

object Build {
  lazy val scalaVersions = Seq("2.12.11", "2.13.1")

  val splainSettings = Seq(
    addCompilerPlugin("io.tryp" % "splain" % "0.5.1" cross CrossVersion.patch),
    scalacOptions ++= Seq(
      "-P:splain:all",
      "-P:splain:foundreq:false",
      "-P:splain:keepmodules:500",
      "-P:splain:rewrite:^((([^\\.]+\\.)*)([^\\.]+))\\.Type$/$1"
    )
  )

  def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String): Seq[java.io.File] =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, 12)) => Seq(srcBaseDir / srcName / "scala-2.12")
      case Some((2, 13)) => Seq(srcBaseDir / srcName / "scala-2.13")
      case _ => Seq()
    }

  val baseSettings = splainSettings ++ Seq(
    organization := "andxor",
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("2.13")).get,
    version := currentVersion,
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
    unmanagedSourceDirectories in Compile ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
    unmanagedSourceDirectories in Test ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
    skip in publish := true,
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile, doc) := Seq()
  )

  val catsVersion = "2.1.1"
  val monocleVersion = "2.0.3"
  val silencerVersion = "1.6.0"
  val scalacheckVersion = "1.14.3"
  val scalacheckDep = "org.scalacheck" %% "scalacheck" % scalacheckVersion

  val silencerSettings = Seq(
    addCompilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
    libraryDependencies += "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
  )

  val commonSettings = baseSettings ++ silencerSettings ++ Seq(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-laws" % catsVersion % "test",
      "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-law" % monocleVersion % "test"
    )
  )

  val publishSettings = Seq(
    skip in publish := false,
    bintrayOrganization := Some("bondlink"),
    bintrayRepository := "andxor",
    bintrayReleaseOnPublish in ThisBuild := false,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
  )

  val testSettings = Seq(libraryDependencies += scalacheckDep % "test")

  def generateBase = Project("generate", file("generate"))
    .settings(commonSettings)
    .settings(Seq(
      name := "andxor-generate",
      resolvers += Resolver.sonatypeRepo("snapshots"),
      crossScalaVersions := crossScalaVersions.value.filter(_.startsWith("2.13")),
      libraryDependencies ++= Seq(
        "com.github.pathikrit" %% "better-files" % "3.8.0",
        "org.scalariform" %% "scalariform" % "0.2.10",
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      ),
      TwirlKeys.templateImports := Seq(),
      bintrayRelease := {}
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
      libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.3"
    ))

  val circeVersion = "0.13.0"
  def circeBase = Project("circe", file("circe"))
    .settings(commonSettings)
    .settings(publishSettings)
    .settings(Seq(
      name := "andxor-circe",
      libraryDependencies ++= Seq(
        "io.circe" %% "circe-core" % circeVersion,
        "io.circe" %% "circe-parser" % circeVersion,
        "io.circe" %% "circe-generic" % circeVersion % "test",
        "io.circe" %% "circe-generic-extras" % circeVersion % "test"
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
    scalacOptions in Test ++= enablePlugin((Compile / Keys.`package`).value, pluginOpts),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  )

  def compilerPlugin(proj: Project, nme: String, pluginOpts: Seq[String]) =
    proj
      .settings(baseSettings)
      .settings(silencerSettings)
      .settings(publishSettings)
      .settings(pluginOptions(pluginOpts))
      .settings(name := nme)

  def annotationPlugin(proj: Project, nme: String, pluginOpts: Seq[String]) =
    compilerPlugin(proj, nme, pluginOpts)
      .settings(sourceGenerators in Compile += Def.task {
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
