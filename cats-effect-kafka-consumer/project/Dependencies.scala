import sbt._

object Dependencies {

  val catsVersion = "2.0.0"

  val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13"

  val catsCore = "org.typelevel" %% "cats-core" % catsVersion

  val catsEffect = "org.typelevel" %% "cats-effect" % catsVersion

  val cats = Seq(catsCore, catsEffect)

  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

  val pureConfig = "com.github.pureconfig" %% "pureconfig" % "0.12.1"

  val kafka = "org.apache.kafka" % "kafka-clients" % "2.2.1"

  val dropwizardMetrics = Seq(
    "metrics-graphite",
    "metrics-core",
    "metrics-jvm"
  ).map("io.dropwizard.metrics" % _ % "4.0.5")

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.2" % Test

  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8" % Test

  val mockitoScala = "org.mockito" %% "mockito-scala" % "1.7.1" % Test

  val dockerTestKit = Seq("com.whisk" %% "docker-testkit-scalatest", "com.whisk" %% "docker-testkit-impl-docker-java").map(_ % "0.9.9" % Test)

}
