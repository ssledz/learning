name := "cats-free-playground"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-language:higherKinds"
)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.1.1"


addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)