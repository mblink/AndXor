lazy val generate = (project in file("./generate")).
  settings(
    organization := "ldr",
    scalaVersion := "2.12.5",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.17",
      "com.github.pathikrit" %% "better-files" % "3.5.0",
      "com.geirsson" %% "scalafmt-core" % "1.6.0-RC1"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8", // yes, this is 2 args
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint:-unused,_",
      "-Yno-adapted-args",
      "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:privates,locals",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Ybackend-parallelism", java.lang.Runtime.getRuntime.availableProcessors.toString,
      "-Ycache-plugin-class-loader:last-modified",
      //"-Xlog-implicits",
      "-Ycache-macro-class-loader:last-modified"),
    name := "ldr-generate",
    TwirlKeys.templateImports := Seq()
  ).enablePlugins(SbtTwirl)

lazy val core = (project in file("./core")).
  settings(
    organization := "ldr",
    scalaVersion := "2.12.5",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.17"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8", // yes, this is 2 args
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint:-unused,_",
      "-Yno-adapted-args",
      "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:privates,locals",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Ybackend-parallelism", java.lang.Runtime.getRuntime.availableProcessors.toString,
      "-Ycache-plugin-class-loader:last-modified",
      //"-Xlog-implicits",
      "-Ycache-macro-class-loader:last-modified"),
    name := "ldr-core"
  )

lazy val root = (project in file("."))
  .dependsOn(core)
  .aggregate(generate, core)
  .enablePlugins(TutPlugin)
