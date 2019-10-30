import sbt._

object Dependencies {

  val doobieVersion = "0.7.0"
  val http4sVersion = "0.20.3"
  val circeVersion = "0.11.1"
  val jsoniterVersion = "0.49.1"
  val catsVersion = "1.6.1"
  val catsEffectVersion = "1.3.1"
  val catsFreeVersion = catsVersion
  val fs2Version = "1.0.5"

  val fs2 = Seq(
    "fs2-core",
    "fs2-io"
  ).map("co.fs2" %% _ % fs2Version)

  val doobie = Seq(
    "doobie-core",
    "doobie-hikari" // HikariCP transactor.
  ).map("org.tpolecat" %% _ % doobieVersion)

  val doobiePostgres = "org.tpolecat" %% "doobie-postgres" % doobieVersion // Postgres driver + type mappings.

  val doobieScalaTest = "org.tpolecat" %% "doobie-scalatest" % doobieVersion % Test // Postgres driver + type mappings.

  val doobieH2 = "org.tpolecat" %% "doobie-h2" % doobieVersion // H2 driver + type mappings.

  val http4s = Seq(
    "http4s-dsl",
    "http4s-blaze-server",
    "http4s-blaze-client"
  ).map("org.http4s" %% _ % http4sVersion)

  val circe = Seq(
    "circe-core",
    "circe-generic",
    "circe-parser",
    "circe-generic-extras"
  ).map("io.circe" %% _ % circeVersion)

  val enumeratumCirce = "com.beachape" %% "enumeratum-circe" % "1.5.21"

  val jsoniter = Seq(
    ("jsoniter-scala-core", Compile),
    ("jsoniter-scala-macros", Provided) // required only in compile-time
  ).map { case (name, scope) =>
    "com.github.plokhotnyuk.jsoniter-scala" %% name % jsoniterVersion % scope
  }

  val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13"

  val catsCore = "org.typelevel" %% "cats-core" % catsVersion

  val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion

  val catsFree = "org.typelevel" %% "cats-free" % catsFreeVersion

  val cats = Seq(catsCore, catsEffect, catsFree)

  val flyway = "org.flywaydb" % "flyway-core" % "5.2.0"

  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val pureConfig = "com.github.pureconfig" %% "pureconfig" % "0.10.0"

  val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.8.2"

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0" % Test

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.7" % Test

  val mockitoScala = "org.mockito" %% "mockito-scala" % "1.3.1" % Test

  val tests = Seq(scalaTest, scalaCheck)

  val jettyServer = Seq(
    "jetty-server",
    "jetty-servlet",
    "jetty-webapp"
  ).map("org.eclipse.jetty" % _ % "9.4.8.v20171121")

  val guiceVersion = "4.1.0"

  val guice = "com.google.inject" % "guice" % guiceVersion

  val guiceScala = "net.codingwell" %% "scala-guice" % "4.1.1"

  val guiceExtensions = Seq(
    "guice-assistedinject",
    "guice-servlet"
  ).map("com.google.inject.extensions" % _ % guiceVersion)
}