import andxor.Build._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val generate: Project = generateBase

lazy val core: Project = coreBase

lazy val scalacheck = scalacheckBase.dependsOn(core)

lazy val argonaut: Project = argonautBase
  .dependsOn(core, scalacheck % "test")

lazy val circe: Project = circeBase
  .dependsOn(core, scalacheck % "test")

lazy val tests: Project = testsBase
  .dependsOn(core, scalacheck, argonaut, circe)

lazy val root: Project = project.in(file("."))
  .settings(commonSettings)
  .settings(
    crossScalaVersions := Seq(),
  )
  .aggregate(generate, core, argonaut, circe, scalacheck, tests)

lazy val docs = project.in(file("andxor-docs"))
  .settings(commonSettings)
  .settings(docsSettings)
  .settings(
    mdocOut := file(".."),
    scalacOptions += "-Wconf:msg=any2stringadd:s"
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
