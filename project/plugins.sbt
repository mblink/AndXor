addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.5.1")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.21")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.20")

resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
addSbtPlugin("bondlink" % "sbt-git-publish" % "0.0.5")
