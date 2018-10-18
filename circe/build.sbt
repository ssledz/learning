name := "circe"

version := "0.1"

scalaVersion := "2.12.7"

val circeVersion = "0.9.3"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-java8",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-generic-extras"
).map(_ % circeVersion)