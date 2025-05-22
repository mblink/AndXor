Global / onChangedBuildSource := ReloadOnSourceChanges

val currentVersion = "0.14.1"

val scala2 = "2.13.16"
val scala3 = "3.3.6"

ThisBuild / crossScalaVersions := Seq(scala2, scala3)

val argonautDep = "io.argonaut" %% "argonaut" % "6.3.10"

val betterFiles = "com.github.pathikrit" %% "better-files" % "3.9.2"

val catsVersion = "2.13.0"
val catsCore = "org.typelevel" %% "cats-core" % catsVersion
val catsLaws = "org.typelevel" %% "cats-laws" % catsVersion % Test

val circeDep = "io.circe" %% "circe-core" % "0.14.13"

val monocleVersion = "3.3.0"
val monocleCore = "dev.optics" %% "monocle-core" % monocleVersion
val monocleLaws = "dev.optics" %% "monocle-law" % monocleVersion % Test

val newtype = "io.estatico" %% "newtype" % "0.4.4"

val scalacheckDep = "org.scalacheck" %% "scalacheck" % "1.18.1"

val scalaReflect = Def.setting("org.scala-lang" % "scala-reflect" % scalaVersion.value)

val scalariform = Seq(
  ("org.scalariform" %% "scalariform" % "0.2.10")
    .cross(CrossVersion.for3Use2_13)
    .exclude("org.scala-lang.modules", "scala-xml_2.13"),
  "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
)

// GitHub Actions config
val javaVersions = Seq(8, 11, 17, 21).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("master")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"
def isScala(v: String) = s"matrix.scala == '$v'"

ThisBuild / githubWorkflowBuild ++= Seq(
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility"), cond = Some(isJava(21))),
  WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Build docs"), cond = Some(isJava(21) ++ " && " ++ isScala(scala2))),
)

def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val baseSettings = Seq(
  organization := "andxor",
  version := currentVersion,
  crossScalaVersions := Seq(scala2, scala3),
  scalaVersion := scala3,
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
      "-Wunused:unsafe-warn-patvars",
    ),
  ),
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full)),
    Seq(),
  ),
)

lazy val publishSettings = Seq(
  publish / skip := false,
  publishTo := Some("BondLink S3".at("s3://bondlink-maven-repo")),
  licenses += License.Apache2,
  resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com",
  mimaPreviousArtifacts := Set(
    "andxor" %% name.value % "0.14.1",
  ),
)

lazy val publishOnlyScala2 = Seq(
  publish / skip := foldScalaV(scalaVersion.value)(false, true),
  mimaPreviousArtifacts := foldScalaV(scalaVersion.value)(mimaPreviousArtifacts.value, Set()),
)

lazy val testSettings = Seq(libraryDependencies += scalacheckDep % Test)

baseSettings

def baseProj(id: String, nme: String) =
  Project(id, file(id)).settings(baseSettings).settings(name := nme)

lazy val generate = baseProj("generate", "andxor-generate")
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

lazy val core = baseProj("core", "andxor-core")
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(catsCore, monocleCore) ++ foldScalaV(scalaVersion.value)(Seq(newtype, scalaReflect.value), Seq()),
  )

lazy val argonaut = baseProj("argonaut", "andxor-argonaut")
  .settings(publishSettings)
  .settings(publishOnlyScala2)
  .settings(testSettings)
  .settings(libraryDependencies += argonautDep)
  .dependsOn(core, scalacheck % Test)

lazy val circe = baseProj("circe", "andxor-circe")
  .settings(publishSettings)
  .settings(publishOnlyScala2)
  .settings(testSettings)
  .settings(libraryDependencies += circeDep)
  .dependsOn(core, scalacheck % Test)

lazy val scalacheck = baseProj("scalacheck", "andxor-scalacheck")
  .settings(publishSettings)
  .settings(libraryDependencies += scalacheckDep)
  .dependsOn(core)

lazy val tests = baseProj("tests", "andxor-tests")
  .settings(libraryDependencies ++= Seq(catsLaws, monocleLaws))
  .dependsOn(core, scalacheck, argonaut, circe)

lazy val docs = project.in(file("andxor-docs"))
  .settings(baseSettings)
  .settings(
    mdocOut := file("."),
    scalacOptions += "-Wconf:msg=any2stringadd:s",
    scalacOptions -= "-Wnonunit-statement",
  )
  .dependsOn(core)
  .enablePlugins(MdocPlugin)
