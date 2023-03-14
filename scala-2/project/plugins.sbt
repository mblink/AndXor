addSbtPlugin("com.typesafe.play" % "sbt-twirl" % "1.5.2")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.6")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.2")

resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
addSbtPlugin("bondlink" % "sbt-git-publish" % "0.0.5")