name := "neo4j-ops"

organization := "org.dots42"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.neo4j.driver" % "neo4j-java-driver" % "1.0.0",
  "org.scalaz" %% "scalaz-core" % "7.2.2",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.2",
  "org.scalaz" %% "scalaz-effect" % "7.2.2",
  "oncue.knobs" %% "core" % "3.6.1a",
  "org.specs2" %% "specs2-core" % "3.7.2" % "test"
)

resolvers += Resolver.bintrayRepo("oncue", "releases")

exportJars := true

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
