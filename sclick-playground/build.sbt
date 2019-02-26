name := "slick-playground"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.3.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "mysql" % "mysql-connector-java" % "8.0.15",
  "com.h2database" % "h2" % "1.4.198",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.3.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)