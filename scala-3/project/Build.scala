package andxor

import java.io.File
import sbt.*
import sbt.Keys.*
import sbtgitpublish.GitPublishKeys.*

object Build extends CommonBuild {
  val scalaVersions = Seq("3.3.1")

  val lintOpts = Seq(
    "-Wvalue-discard",
    "-Wunused:implicits",
    "-Wunused:imports",
    "-Wunused:locals",
    "-Wunused:params",
    "-Wunused:privates",
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
