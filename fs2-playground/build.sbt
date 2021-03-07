import Dependencies._

name := "fs2-playground"

scalaVersion := "2.13.5"

addCommandAlias("fmt", ";scalafmt ;test:scalafmt ;scalafmtSbt")

// https://github.com/typelevel/kind-projector
lazy val typeSystemEnhancements = compilerPlugin(Library.kindProjector cross CrossVersion.full)

// https://github.com/oleg-py/better-monadic-for
lazy val betterMonadicFor = compilerPlugin(Library.betterMonadicFor)


libraryDependencies ++= Seq(
  typeSystemEnhancements,
  betterMonadicFor,
  Library.fs2core,
  Library.fs2io
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "utf-8",
  "-explaintypes",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Ywarn-unused:_",
  "-Ymacro-annotations"
)

enablePlugins(ScalafmtPlugin)