addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.5.0")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.1.5")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.16")

resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
addSbtPlugin("bondlink" % "sbt-git-publish" % "0.0.3")
