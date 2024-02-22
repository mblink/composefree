addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.23.0")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.5.2")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.0")

resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
addSbtPlugin("bondlink" % "sbt-git-publish" % "0.0.5")
