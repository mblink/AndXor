Global / onChangedBuildSource := ReloadOnSourceChanges

val currentVersion = "0.16.0"

val scala2 = "2.13.18"
val scala3 = "3.3.8"

ThisBuild / scalaVersion := scala3

val argonautDep = "io.github.argonaut-io" %% "argonaut" % "6.3.13"

val betterFiles = "com.github.pathikrit" %% "better-files" % "3.9.2"

val catsVersion = "2.13.0"
val catsCore = "org.typelevel" %% "cats-core" % catsVersion
val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % Test

val circeDep = "io.circe" %% "circe-core" % "0.14.15"

val monocleVersion = "3.3.0"
val monocleCore = "dev.optics" %% "monocle-core" % monocleVersion
val monocleLaws = "dev.optics" %% "monocle-law" % monocleVersion % Test

val newtype = "io.estatico" %% "newtype" % "0.4.4"

val scalacheckDep = "org.scalacheck" %% "scalacheck" % "1.19.0"

val scalaReflect = Def.setting("org.scala-lang" % "scala-reflect" % scalaVersion.value)

val scalariform = Seq(
  ("org.scalariform" %% "scalariform" % "0.2.10")
    .cross(CrossVersion.for3Use2_13)
    .exclude("org.scala-lang.modules", "scala-xml_2.13"),
  "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
)

// GitHub Actions config
val javaVersions = Seq(17, 21, 25).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("master")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()
ThisBuild / githubWorkflowUseSbtThinClient := true

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(List("sbt test"), name = Some("Build project")),
  WorkflowStep.Run(List("sbt mimaReportBinaryIssues"), name = Some("Check binary compatibility"), cond = Some(isJava(25))),
  WorkflowStep.Run(List("sbt mdoc"), name = Some("Build docs"), cond = Some(isJava(25))),
)

def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val baseSettings = Seq(
  organization := "andxor",
  version := currentVersion,
  publish / skip  := true,
  Compile / packageDoc / publishArtifact := false,
  packageDoc / publishArtifact := false,
  Compile / doc / sources := Seq(),
  mimaPreviousArtifacts := Set(),
  scalacOptions ++= foldScalaV(scalaVersion.value)(
    Seq(
      "-Vimplicits",
      "-Vimplicits-verbose-tree",
      "-Xlint:strict-unsealed-patmat",
      "-Ymacro-annotations",
    ),
    Seq(
      "-explain",
      "-no-indent",
    ),
  ),
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.full)),
    Seq(),
  ),
)

lazy val publishSettings = Seq(
  publish / skip := false,
  s3PublishBucket := "bondlink-maven-repo",
  licenses += License.Apache2,
  resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com",
  mimaPreviousArtifacts := Set(
    organization.value %% name.value % "0.16.0",
  ),
)

lazy val publishOnlyScala2 = Seq(
  publish / skip := foldScalaV(scalaVersion.value)(false, true),
  mimaPreviousArtifacts := foldScalaV(scalaVersion.value)(mimaPreviousArtifacts.value, Set()),
)

lazy val testSettings = Seq(libraryDependencies += scalacheckDep % Test)

baseSettings

def baseProj(matrix: ProjectMatrix, nme: String) =
  matrix
    .jvmPlatform(scalaVersions = Seq(scala2, scala3))
    .settings(baseSettings ++ Seq(name := nme))

lazy val generate = baseProj(projectMatrix.in(file("generate")), "andxor-generate")
  .settings(
    libraryDependencies ++= scalariform ++ foldScalaV(scalaVersion.value)(
      Seq(betterFiles, catsCore, scalaReflect.value),
      Seq(),
    ),
    buildInfoKeys := Seq[BuildInfoKey]("rootDir" -> (ThisBuild / baseDirectory).value.toString),
    buildInfoPackage := "andxor",
    TwirlKeys.templateImports := Seq(),
    Compile / TwirlKeys.compileTemplates / sourceDirectories := foldScalaV(scalaVersion.value)(
      Seq((Compile / sourceDirectory).value / "twirl-2"),
      Seq(),
    ),
    Test / TwirlKeys.compileTemplates / sourceDirectories := Seq(),
  )
  .enablePlugins(BuildInfoPlugin, SbtTwirl)

lazy val core = baseProj(projectMatrix.in(file("core")), "andxor-core")
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(catsCore, monocleCore) ++ foldScalaV(scalaVersion.value)(Seq(newtype, scalaReflect.value), Seq()),
  )

lazy val argonaut = baseProj(projectMatrix.in(file("argonaut")), "andxor-argonaut")
  .settings(publishSettings)
  .settings(publishOnlyScala2)
  .settings(testSettings)
  .settings(libraryDependencies += argonautDep)
  .dependsOn(core, scalacheck % Test)

lazy val circe = baseProj(projectMatrix.in(file("circe")), "andxor-circe")
  .settings(publishSettings)
  .settings(publishOnlyScala2)
  .settings(testSettings)
  .settings(libraryDependencies += circeDep)
  .dependsOn(core, scalacheck % Test)

lazy val scalacheck = baseProj(projectMatrix.in(file("scalacheck")), "andxor-scalacheck")
  .settings(publishSettings)
  .settings(libraryDependencies += scalacheckDep)
  .dependsOn(core)

lazy val tests = baseProj(projectMatrix.in(file("tests")), "andxor-tests")
  .settings(libraryDependencies ++= Seq(catsLaws, monocleLaws))
  .dependsOn(core, scalacheck, argonaut, circe)

lazy val docs = projectMatrix.in(file("andxor-docs"))
  .jvmPlatform(scalaVersions = Seq(scala3))
  .settings(baseSettings)
  .settings(
    mdocOut := file("."),
    scalacOptions += "-Wconf:msg=any2stringadd:s",
    scalacOptions -= "-Wnonunit-statement",
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
