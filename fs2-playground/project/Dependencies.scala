import sbt._

object Dependencies {

  object Version {
    val fs2              = "2.5.0"
    val betterMonadicFor = "0.3.1"
    val kindProjector    = "0.11.3"
  }

  object Library {

    private def fs2(artifact: String): ModuleID = "co.fs2" %% artifact % Version.fs2

    val fs2core         = fs2("fs2-core")
    val fs2io           = fs2("fs2-io")
    val fs2reactive     = fs2("fs2-reactive-streams")
    val fs2experimental = fs2("fs2-experimental")

    // Compiler plugins
    val betterMonadicFor = "com.olegpy"   %% "better-monadic-for" % Version.betterMonadicFor
    val kindProjector    = "org.typelevel" % "kind-projector"     % Version.kindProjector

  }

}
