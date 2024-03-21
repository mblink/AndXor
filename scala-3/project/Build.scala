package andxor

import java.io.File
import sbt.*
import sbt.Keys.*
import sbtgitpublish.GitPublishKeys.*

object Build extends CommonBuild {
  val relDir = "scala-3"
  val scalaVersions = Seq(scala3)

  val lintOpts = Seq(
    "-Wunused:unsafe-warn-patvars",
  )

  val baseSettings0 = Seq(
    crossScalaVersions := scalaVersions,
    scalaVersion := scalaVersions.find(_.startsWith("3.")).get,
    scalacOptions ++= lintOpts ++ Seq(
      "-explain",
      "-no-indent",
    ),
  )

  override def testsBase = super.testsBase.settings(scalacOptions --= lintOpts)
}
