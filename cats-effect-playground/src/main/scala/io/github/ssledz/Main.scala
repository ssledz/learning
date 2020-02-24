package io.github.ssledz

import cats.effect._
import cats.syntax.all._

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = IO(println("Hello World")).as(ExitCode.Success)
}
