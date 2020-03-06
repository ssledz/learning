package io.github.ssledz

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.github.ssledz.kafka._
object ConsoleConsumerApp extends IOApp with KafkaConsumerProcessor {

  private implicit val errorHandler = new LogOnlyErrorHandler[IO]

  def run(args: List[String]): IO[ExitCode] =
    Config.load match {
      case Right(c) =>
        for {
          _ <- IO(logger.info("Configuration:\n{}", c.show))
          status <- run(c)
        } yield status
      case Left(errors) => IO(logger.error("Configuration is broken because: " + errors)).as(ExitCode(1))
    }

  def run(cfg: Config)(implicit errorHandler: ConsumingErrorHandler[IO]): IO[ExitCode] =
    ThreadResources.kafka.use { implicit kafkaBlocker =>
      KafkaConsumerProcessor.kafka[IO](cfg.consumer).use { consumer =>
        pollLoop(cfg.consumer, consumer) { record =>
          IO(println(record))
        }
      }
    }.as(ExitCode.Success)
}
