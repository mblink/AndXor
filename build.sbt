lazy val scalaVersions = Seq("2.12.8", "2.13.0")

lazy val splainSettings = Seq(
  addCompilerPlugin("io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch),
  scalacOptions ++= Seq(
    "-P:splain:all",
    "-P:splain:foundreq:false",
    "-P:splain:keepmodules:500",
    "-P:splain:rewrite:^((([^\\.]+\\.)*)([^\\.]+))\\.Type$/$1"
  )
)

lazy val scala212_opts = Seq(
  "-Xfuture",
  "-Xlint:by-name-right-associative",
  "-Xlint:unsound-match",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit"
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

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String): Seq[java.io.File] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 12)) => Seq(srcBaseDir / srcName / "scala-2.12")
    case Some((2, 13)) => Seq(srcBaseDir / srcName / "scala-2.13")
    case _ => Seq()
  }

lazy val baseSettings = splainSettings ++ Seq(
  organization := "andxor",
  crossScalaVersions := scalaVersions,
  scalaVersion := scalaVersions.find(_.startsWith("2.12")).get,
  version := "0.3.7",
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
    case Some((2, 12)) => scala212_opts ++ scala212_213_opts
    case Some((2, 13)) => scala212_213_opts
    case _ => Seq()
  }),
  scalacOptions in (Compile, console) := scalacOptions.value.filterNot(x =>
    x.startsWith("-Ywarn-unused") || x.startsWith("-Xlint") || x.startsWith("-P:splain")),
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  unmanagedSourceDirectories in Compile ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
  unmanagedSourceDirectories in Test ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
  skip in publish := true,
  publishArtifact in (Compile, packageDoc) := false,
  publishArtifact in packageDoc := false,
  sources in (Compile, doc) := Seq()
)

lazy val scalazVersion = "7.2.27"
lazy val scalacheckVersion = "1.14.0"
lazy val scalacheckDep = "org.scalacheck" %% "scalacheck" % scalacheckVersion

lazy val commonSettings = baseSettings ++ Seq(libraryDependencies += "org.scalaz" %% "scalaz-core" % scalazVersion)

lazy val publishSettings = Seq(
  skip in publish := false,
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "andxor",
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    scalacheckDep % "test",
    "org.scalaz" %% "scalaz-scalacheck-binding" % s"$scalazVersion-scalacheck-${scalacheckVersion.split('.').dropRight(1).mkString(".")}" % "test"
  ),
  testOptions in Test ++= Seq(Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")) ++
    Option(System.getProperty("testIterations")).map(_.toInt)
      .map(n => Seq(Tests.Argument(TestFrameworks.ScalaCheck, "-minSuccessfulTests", n.toString)))
      .getOrElse(Seq())
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
    TwirlKeys.templateImports := Seq(),
    bintrayRelease := {}
  )).enablePlugins(SbtTwirl)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(testSettings)
  .settings(Seq(
    name := "andxor-core",
    scalacOptions ++= enablePlugin((Keys.`package` in Compile in newtype).value, Seq())
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

lazy val scalacheck = project.in(file("scalacheck"))
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(Seq(
    name := "andxor-scalacheck",
    libraryDependencies += scalacheckDep
  ))
  .dependsOn(core)

def pluginOptions(pluginOpts: Seq[String]) = Seq(
  scalacOptions -= "-Ywarn-unused:patvars",
  scalacOptions in Test ++= enablePlugin((Keys.`package` in Compile).value, pluginOpts),
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
)

def enablePlugin(jar: File, extra: Seq[String]): Seq[String] =
  Seq(s"-Xplugin:${jar.getAbsolutePath}", s"-Jdummy=${jar.lastModified}") ++ extra

def compilerPlugin(proj: Project, nme: String, pluginOpts: Seq[String]) =
  proj
    .settings(baseSettings)
    .settings(publishSettings)
    .settings(pluginOptions(pluginOpts))
    .settings(name := nme)

lazy val deriving =
  compilerPlugin(project.in(file("deriving")), "andxor-deriving", Seq(
    "-P:deriving:covariant:Arbitrary",
    "-P:deriving:labelledCovariant:Decoder|DecodeJson|Read",
    "-P:deriving:contravariant:Prod:Csv|Equal",
    "-P:deriving:labelledContravariant:Cop:Csv|Equal",
    "-P:deriving:labelledContravariant:Encoder|EncodeJson|Show"
  ))
    .settings(testSettings)
    .dependsOn(argonaut % "test->test", circe % "test->test", scalacheck % "test->test")

lazy val newtype = compilerPlugin(project.in(file("newtype")), "andxor-newtype", Seq())

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(Seq(
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value,
    bintrayRelease := {}
  ))
  .dependsOn(core)
  .aggregate(generate, core, argonaut, circe, deriving, newtype)
  .enablePlugins(TutPlugin)
