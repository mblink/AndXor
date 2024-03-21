import andxor.Build._

Global / onChangedBuildSource := ReloadOnSourceChanges

githubActionsSettings

lazy val generate: Project = generateBase
  .settings(
    buildInfoKeys := Seq[BuildInfoKey]("rootDir" -> (ThisBuild / baseDirectory).value.toString),
    buildInfoPackage := "andxor",
  )
  .enablePlugins(BuildInfoPlugin)

lazy val core: Project = coreBase

lazy val scalacheck = scalacheckBase.dependsOn(core)

lazy val tests: Project = testsBase
  .dependsOn(core, scalacheck)

lazy val root: Project = project.in(file("."))
  .settings(baseSettings)
  .settings(gitRelease := {})
  .aggregate(core, scalacheck, tests)

lazy val docs = project.in(file("andxor-docs"))
  .settings(commonSettings)
  .settings(
    mdocOut := file("."),
    scalacOptions += "-Wconf:msg=any2stringadd:s"
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
