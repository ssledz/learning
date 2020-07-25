name := "eclipse-link-playground"

version := "0.1"

scalaVersion := "2.13.3"

val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"
val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
val julToSlf = "org.slf4j" % "jul-to-slf4j" % "1.7.26"
val jpa = "org.eclipse.persistence" % "org.eclipse.persistence.jpa" % "2.7.7"
val jpaExt = "org.eclipse.persistence" % "org.eclipse.persistence.extension" % "2.7.7"
val mysql = "mysql" % "mysql-connector-java" % "8.0.21"

libraryDependencies ++= Seq(logback, logging, julToSlf, jpaExt, jpa, mysql)