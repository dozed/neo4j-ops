name := "neo4j-ops"

organization := "org.dots42"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.neo4j" % "neo4j" % "2.3.2",
  "org.neo4j" % "neo4j-shell" % "2.3.2",
  "org.scalaz" %% "scalaz-core" % "7.2.2",
  "org.slf4j" % "slf4j-api" % "1.7.13",
  "org.specs2" %% "specs2-core" % "3.8.9" % "test"
)

exportJars := true

// https://github.com/non/kind-projector
// makes writing type signatures easier
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
