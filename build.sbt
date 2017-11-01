scalaVersion := "2.12.2"

libraryDependencies += "org.scalameta" %% "scalameta" % "1.7.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.scalameta" % "paradise_2.12.2" % "3.0.0-M8")
