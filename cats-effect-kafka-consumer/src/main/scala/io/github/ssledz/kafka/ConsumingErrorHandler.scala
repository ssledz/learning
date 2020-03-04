package io.github.ssledz.kafka

import cats.effect.{ContextShift, Sync}
import cats.{Monad, Parallel}
import io.github.ssledz.kafka.KafkaConsumerProcessor._
import org.slf4j.LoggerFactory

trait ConsumingErrorHandler[F[_]] {
  def handleErrors(errors: List[ConsumingError]): F[Unit]
}

trait ErrorProcessor[F[_]] {
  def processError(error: ConsumingError): F[Option[ConsumingError]]
}

import cats.implicits._

class CompoundErrorHandler[F[_]: Monad: Sync: ContextShift: Parallel](processors: List[ErrorProcessor[F]]) extends ConsumingErrorHandler[F] {

  private val processor: ErrorProcessor[F] = processors.reduce(compose)

  private val logOnlyHandler = new LogOnlyErrorHandler[F]

  private def compose(p1: ErrorProcessor[F], p2: ErrorProcessor[F]): ErrorProcessor[F] = new ErrorProcessor[F] {
    override def processError(error: ConsumingError): F[Option[ConsumingError]] =
      p1.processError(error).flatMap {
        case Some(err) => p2.processError(err)
        case None => Monad[F].pure(None)
      }

  }

  def handleErrors(errors: List[ConsumingError]): F[Unit] =
    for {
      errors <- errors.map(processor.processError).parSequence.map(_.flatten)
      _ <- logOnlyHandler.handleErrors(errors)
    } yield ()

}

class LogOnlyErrorHandler[F[_]: Sync] extends ConsumingErrorHandler[F] {

  private val logger = LoggerFactory.getLogger(getClass)

  def handleErrors(errors: List[ConsumingError]): F[Unit] = Sync[F].delay {
    errors.foreach(err => logger.error(s"Unknown error during processing record : ${err.record}", err.exception))
  }

}
