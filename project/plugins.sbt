resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com"

addSbtPlugin("bondlink" % "sbt-s3-publish" % "0.0.1")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.11.0")
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.30.0")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.9.0")
addSbtPlugin("org.typelevel" % "sbt-tpolecat" % "0.5.6")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.6")
