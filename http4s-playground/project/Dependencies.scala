import sbt._

object Dependencies {

  val DoobieVersion = "0.9.0"
  val CatsVersion = "2.1.1"
  val CatsEffectVersion = "2.1.3"
  val CirceVersion = "0.13.0"
  val CirceConfigVersion = "0.8.0"
  val EnumeratumCirceVersion = "1.6.1"
  val EnumeratumVersion = "1.6.1"
  val Http4sVersion = "0.21.4"
  val Fs2Version = "2.4.2"
  val FlywayVersion = "6.4.4"

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % Fs2Version,
    "co.fs2" %% "fs2-io" % Fs2Version
  )

  val doobie = Seq(
    "org.tpolecat" %% "doobie-core" % DoobieVersion,
    "org.tpolecat" %% "doobie-hikari" % DoobieVersion
  )

  val doobieH2 = "org.tpolecat" %% "doobie-h2" % DoobieVersion

  val doobiePostgres = "org.tpolecat" %% "doobie-postgres" % DoobieVersion

  val doobieScalaTest = "org.tpolecat" %% "doobie-scalatest" % DoobieVersion % Test

  val flywaydb = "org.flywaydb" % "flyway-core" % FlywayVersion

  val http4s = Seq(
    "org.http4s" %% "http4s-blaze-server" % Http4sVersion,
    "org.http4s" %% "http4s-circe" % Http4sVersion,
    "org.http4s" %% "http4s-dsl" % Http4sVersion
  )

  val enumeratumCirce = "com.beachape" %% "enumeratum-circe" % EnumeratumCirceVersion

  val circe = Seq(
    "io.circe" %% "circe-core" % CirceVersion,
    "io.circe" %% "circe-generic" % CirceVersion,
    "io.circe" %% "circe-parser" % CirceVersion,
    "io.circe" %% "circe-generic-extras" % CirceVersion,
    "io.circe" %% "circe-config" % CirceConfigVersion,
    enumeratumCirce
  )

  val enumeratum = "com.beachape" %% "enumeratum" % EnumeratumVersion

  val catsCore = "org.typelevel" %% "cats-core" % CatsVersion

  val catsEffect = "org.typelevel" %% "cats-effect" % CatsEffectVersion

  val catsFree = "org.typelevel" %% "cats-free" % CatsVersion

  val cats = Seq(catsCore, catsEffect, catsFree)

  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.2" % Test

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8" % Test

}
