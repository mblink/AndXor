lazy val commonSettings = Seq(
  organization := "andxor",
  scalaVersion := "2.12.8",
  version := "0.2.4",
  libraryDependencies ++= Seq("org.scalaz" %% "scalaz-core" % "7.2.26"),
  addCompilerPlugin("io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-explaintypes",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-P:splain:all",
    "-P:splain:rewrite:andxor\\.types\\.((Cop|Prod)\\d+)\\.Type/$1",
    "-Xcheckinit",
    "-Xfatal-warnings",
    "-Xfuture",
    "-Xlint:adapted-args",
    "-Xlint:by-name-right-associative",
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
    "-Xlint:unsound-match",
    "-Yno-adapted-args",
    "-Ypartial-unification",
    "-Ywarn-dead-code",
    "-Ywarn-extra-implicit",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused:implicits",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:params",
    "-Ywarn-unused:patvars",
    "-Ywarn-unused:privates",
    "-Ywarn-value-discard",
    "-Ycache-plugin-class-loader:last-modified",
    "-Ycache-macro-class-loader:last-modified"
  ),
  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
  skip in publish := true
)

lazy val publishSettings = Seq(
  skip in publish := false,
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "andxor",
  bintrayReleaseOnPublish in ThisBuild := false,
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

lazy val generate = project.in(file("generate"))
  .settings(commonSettings ++ Seq(
    name := "andxor-generate",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies ++= Seq(
      "com.github.pathikrit" %% "better-files" % "3.5.0",
      "org.scalameta" %% "scalafmt-core" % "2.0.0-RC6",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    TwirlKeys.templateImports := Seq()
  )).enablePlugins(SbtTwirl)

lazy val core = project.in(file("core"))
  .settings(commonSettings ++ publishSettings ++ Seq(
    name := "andxor-core",
    libraryDependencies += "io.estatico" %% "newtype" % "0.4.2",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
  ))

lazy val root = project.in(file("."))
  .settings(commonSettings ++ Seq(
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value
  ))
  .dependsOn(core)
  .aggregate(generate, core)
  .enablePlugins(TutPlugin)
