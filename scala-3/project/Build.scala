package andxor

import java.io.File
import sbt.*
import sbt.Keys.*

object Build extends CommonBuild {
  val scalaVersions = Seq("3.3.4")

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
