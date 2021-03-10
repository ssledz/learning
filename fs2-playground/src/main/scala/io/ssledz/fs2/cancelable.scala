package io.ssledz.fs2

import cats.effect.{Concurrent, ExitCode, IO, IOApp, Sync, Timer}
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import concurrent.duration._

object cancelable extends IOApp {

  def cancelableTask: Promise[Int] = {
    val promise: Promise[Int] = Promise()
    Future {
      while (!promise.isCompleted) {
        val num = Random.nextInt(100000)
        if (num == 12345) {
          promise.tryComplete(Try(num))
        } else {
          println("Crunching numbers")
          Thread.sleep(500)
        }
      }
      println("Completed")
      promise.tryComplete(Try(1))
    }
    promise
  }

  def asyncFrom[F[_]: Concurrent](task: Promise[Int]): F[Int] = {
    Concurrent[F].cancelable[Int] { cb =>
      task.future.onComplete {
        case Failure(exception) => cb(Left(exception))
        case Success(value)     => cb(Right(value))
      }
      Sync[F].delay(task.tryFailure(new RuntimeException("Canceled"))) *> Sync[F].delay(println("Canceled"))
    }
  }

  val timeout: IO[Unit] = Timer[IO].sleep(2000.millis)

  def run(args: List[String]): IO[ExitCode] =
    for {
      res <- IO.race(asyncFrom[IO](cancelableTask), timeout)
      _ <- IO(println(s"result: $res"))
      _ <- IO.never
    } yield ExitCode.Success
}
