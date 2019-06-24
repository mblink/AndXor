lazy val scalaVersions = Seq("2.11.12", "2.12.8", "2.13.0")

lazy val splainSettings = Seq(
  addCompilerPlugin("io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch),
  scalacOptions ++= Seq(
    "-P:splain:all",
    "-P:splain:keepmodules:500",
    "-P:splain:rewrite:^((([^\\.]+\\.)*)([^\\.]+))\\.Type$/$1"
  )
)

lazy val scala211_212_opts = Seq(
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit"
)

lazy val scala212_opts = Seq(
  "-Xlint:by-name-right-associative",
  "-Xlint:unsound-match"
)

lazy val scala212_213_opts = Seq(
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-override",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:imports",
  "-Ywarn-unused:locals",
  "-Ywarn-unused:params",
  "-Ywarn-unused:patvars",
  "-Ywarn-unused:privates",
  "-Ywarn-extra-implicit",
  "-Ycache-plugin-class-loader:last-modified",
  "-Ycache-macro-class-loader:last-modified"
)

lazy val baseSettings = splainSettings ++ Seq(
  organization := "andxor",
  crossScalaVersions := scalaVersions,
  scalaVersion := scalaVersions.find(_.startsWith("2.12")).get,
  version := "0.2.5-LOCAL-40",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-explaintypes",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xcheckinit",
    "-Xfatal-warnings",
    "-Yrangepos",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) => Seq("-Xlint", "-Ywarn-unused") ++ scala211_212_opts
    case Some((2, 12)) => scala211_212_opts ++ scala212_opts ++ scala212_213_opts
    case Some((2, 13)) => scala212_213_opts
    case _ => Seq()
  }),
  scalacOptions in (Compile, console) --= scalacOptions.value.filterNot(x => x.startsWith("-Ywarn-unused") || x.startsWith("-Xlint")),
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  skip in publish := true,
  publishArtifact in (Compile, packageDoc) := false,
  publishArtifact in packageDoc := false,
  sources in (Compile, doc) := Seq()
)

lazy val scalazVersion = "7.2.27"
lazy val scalaCheckVersion = "1.14.0"

lazy val commonSettings = baseSettings ++ Seq(libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion)

lazy val publishSettings = Seq(
  skip in publish := false,
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "andxor",
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val generate = project.in(file("generate"))
  .settings(commonSettings)
  .settings(Seq(
    name := "andxor-generate",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    crossScalaVersions := Seq(),
    libraryDependencies ++= Seq(
      "com.github.pathikrit" %% "better-files" % "3.5.0",
      "org.scalariform" %% "scalariform" % "0.2.10",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    TwirlKeys.templateImports := Seq()
  )).enablePlugins(SbtTwirl)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(Seq(
    name := "andxor-core",
    scalacOptions ++= enablePlugin((assembly in newtype).value),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
      "org.scalaz" %% "scalaz-scalacheck-binding" % s"$scalazVersion-scalacheck-${scalaCheckVersion.split('.').dropRight(1).mkString(".")}" % "test"
    )
  ))

lazy val argonaut = project.in(file("argonaut"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(Seq(
    name := "andxor-argonaut",
    libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.3"
  ))
  .dependsOn(core)

lazy val circeVersion = "0.12.0-M3"
lazy val circe = project.in(file("circe"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(Seq(
    name := "andxor-circe",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion)
  ))
  .dependsOn(core)

lazy val scalametaV = "4.1.12"

lazy val basePluginOptions = Seq(
  scalacOptions -= "-Ywarn-unused:patvars",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    "org.scalameta" %% "scalameta" % scalametaV,
    "org.scalameta" %% "semanticdb-scalac-core" % scalametaV cross CrossVersion.full
  )
)

def enablePlugin(jar: File): Seq[String] = Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}")

lazy val pluginOptions = basePluginOptions ++ Seq(
  scalacOptions in (Compile, console) ++= enablePlugin(assembly.value),
  scalacOptions in Test ++= enablePlugin(assembly.value),
  test.in(assembly) := {},
  assemblyJarName.in(assembly) :=
    name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
  assemblyOption.in(assembly) ~= { _.copy(includeScala = false) },
  Keys.`package`.in(Compile) := {
    val slimJar = Keys.`package`.in(Compile).value
    val fatJar = new File(crossTarget.value + "/" + assemblyJarName.in(assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), CopyOptions().withOverwrite(true))
    slimJar
  },
  packagedArtifact.in(Compile).in(packageBin) := {
    val temp = packagedArtifact.in(Compile).in(packageBin).value
    val (art, slimJar) = temp
    val fatJar = new File(crossTarget.value + "/" + assemblyJarName.in(assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), CopyOptions().withOverwrite(true))
    (art, slimJar)
  },
  assemblyMergeStrategy.in(assembly) := {
    case PathList("com", "sun", _*) => MergeStrategy.discard
    case PathList("sun", _*) => MergeStrategy.discard
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
)

def compilerPlugin(proj: Project, nme: String) =
  proj
    .settings(baseSettings)
    .settings(publishSettings)
    .settings(pluginOptions)
    .settings(name := nme)

lazy val deriving =
  compilerPlugin(project.in(file("deriving")), "andxor-deriving")
    .dependsOn(argonaut % "test->test", circe % "test->test")

lazy val newtype = compilerPlugin(project.in(file("newtype")), "andxor-newtype")

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(Seq(
    crossScalaVersions := Seq(),
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value
  ))
  .dependsOn(core)
  .aggregate(generate, core, argonaut, circe, deriving, newtype)
  .enablePlugins(TutPlugin)
