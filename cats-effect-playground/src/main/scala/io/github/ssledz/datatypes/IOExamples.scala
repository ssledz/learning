package io.github.ssledz.datatypes

import cats.effect.{ContextShift, ExitCode, IO, IOApp}
import cats.implicits._

object IOExamples extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
//    StackSafety.app.as(ExitCode.Success)
    DescribingEffects.app.as(ExitCode.Success)

}

object DescribingEffects {

  val helloWorld = IO(println("Hello World"))

  val thisIsWrong = IO.pure(println("THIS IS WRONG!"))

  def app(implicit cs: ContextShift[IO]) =
    for {
      _ <- helloWorld
      _ <- thisIsWrong
      _ <- IO.pure(25).flatMap(n => IO(println(s"Number is: $n")))
      neverTask <- (IO.never *> IO(println("This will be never called !"))).start
      _ <- IO.unit
      _ <- IO.race(IO(Thread.sleep(1000)) *> IO(println("Time out !!!")), neverTask.join)
    } yield ()

}

object StackSafety {

  val app = for {
    fib4 <- StackSafety.fib(4)
    fib5 <- StackSafety.fib2(5)
    _ <- IO(println(s"fib4: $fib4"))
    _ <- IO(println(s"fib5: $fib5"))
  } yield ()

  def fib(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO(a + b).flatMap { b2 =>
      if (n > 0)
        fib(n - 1, b, b2)
      else
        IO.pure(a)
    }

  def fib2(n: Int, a: Long = 0, b: Long = 1): IO[Long] =
    IO.suspend {
      if (n > 0)
        fib(n - 1, b, a + b)
      else
        IO.pure(a)
    }
}
