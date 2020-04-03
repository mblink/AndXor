import andxor.Build._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val generate: Project = generateBase

lazy val core: Project = coreBase
  .settings(scalacOptions ++= enablePlugin((newtypeBase / Compile / Keys.`package`).value, Seq()))

lazy val scalacheck = scalacheckBase.dependsOn(core)

lazy val argonaut: Project = argonautBase
  .settings(scalacOptions in Test ++= enablePlugin((derivingBase / Compile / Keys.`package`).value, derivingFlags))
  .dependsOn(core, scalacheck % "test")

lazy val circe: Project = circeBase
  .settings(scalacOptions in Test ++= enablePlugin((derivingBase / Compile / Keys.`package`).value, derivingFlags))
  .dependsOn(core, scalacheck % "test")

lazy val deriving: Project = derivingBase
  .settings(scalacOptions in Test ++= enablePlugin((newtype / Compile / Keys.`package`).value, Seq()))
  .dependsOn(argonaut % "test", circe % "test", scalacheck % "test")

lazy val newtype: Project = newtypeBase

lazy val root: Project = project.in(file("."))
  .settings(commonSettings)
  .settings(crossScalaVersions := Seq())
  .dependsOn(core)
  .aggregate(generate, core, argonaut, circe, scalacheck, deriving, newtype)

lazy val docs = project.in(file("andxor-docs"))
  .settings(mdocOut := file("."))
  .dependsOn(root)
  .enablePlugins(MdocPlugin)
