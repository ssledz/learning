package io.github.ssledz.kafka

import cats.effect.{ContextShift, IO}
import io.github.ssledz.kafka.KafkaConsumerProcessor._
import org.slf4j.LoggerFactory

trait ConsumingErrorHandler {

  def handleErrors(errors: List[ConsumingError]): IO[Unit]

}

object ConsumingErrorHandler {}

trait ErrorProcessor {

  def processError(error: ConsumingError): IO[Option[ConsumingError]]

}

class ProcessorConsumingErrorHandler(processors: List[ErrorProcessor])(implicit cs: ContextShift[IO]) extends ConsumingErrorHandler {

  private val processor = processors.reduce(compose)

  private val logOnlyHandler = new LogOnlyErrorHandler

  private def compose(p1: ErrorProcessor, p2: ErrorProcessor): ErrorProcessor = new ErrorProcessor {
    override def processError(error: ConsumingError): IO[Option[ConsumingError]] =
      p1.processError(error).flatMap {
        case Some(err) => p2.processError(err)
        case None => IO.pure(None)
      }

  }

  def handleErrors(errors: List[ConsumingError]): IO[Unit] = {

    import cats.implicits._

    val x = errors.map(processor.processError)

    for {
      errors <- x.parSequence.map(_.flatten)
      _ <- logOnlyHandler.handleErrors(errors)
    } yield ()

  }

}

class LogOnlyErrorHandler extends ConsumingErrorHandler {

  private val logger = LoggerFactory.getLogger(getClass)

  def handleErrors(errors: List[ConsumingError]): IO[Unit] = IO {
    errors.foreach {
      case UnknownError(record, exception) =>
        logger.error(s"Unknown error during processing record : $record", exception)
    }
  }

}
