package io.github.ssledz.kafka

import java.lang

import cats.Monad
import cats.effect.{Blocker, IO}
import io.github.ssledz.kafka.Config.{KafkaConsumerConfig, KafkaPollConfig}
import io.github.ssledz.kafka.KafkaConsumerProcessor.ConsumingError
import io.github.ssledz.kafka.KafkaConsumerProcessorSpec._
import io.github.ssledz.kafka.ThreadResources.KafkaBlocker
import org.apache.kafka.clients.consumer.{ConsumerRecord, MockConsumer, OffsetResetStrategy}
import org.apache.kafka.common.TopicPartition
import org.scalatest.FlatSpec

import scala.jdk.CollectionConverters._

class KafkaConsumerProcessorSpec extends FlatSpec with KafkaConsumerProcessor {

  implicit val kafkaBlocker = KafkaBlocker(Blocker.liftExecutionContext(concurrent.ExecutionContext.global))
  implicit val cs = IO.contextShift(concurrent.ExecutionContext.global)

  behavior.of("pollLoop when for a given record, error occurs")

  behave.like(processRecord(_ => IO.raiseError(FixedError), List(ConsumingError(FixedRecord, FixedError))))

  behavior.of("pollLoop when happy path")

  behave.like(processRecord(_ => IO.pure(Right(())), List.empty))

  def processRecord(f: String => IO[Unit], forwardedErrors: List[ConsumingError], record: String = FixedRecord): Unit = {

    val message = if (forwardedErrors.isEmpty) "process record with no error" else s"forward $forwardedErrors to error handler"

    it should message in {

      // given

      val errorProcessor = new AggregateErrorProcessor[IO]

      implicit val consumerErrorHandler: CompoundErrorHandler[IO] = new CompoundErrorHandler[IO](List(errorProcessor))

      val consumer = newMockConsumer(TestConfig)

      addRecord(consumer, TestConfig.topic, record)

      // when

      val loop = pollLoop[IO](TestConfig, consumer)(f)

      val actual = loop.attempt.unsafeRunSync()

      // then

      assert(actual === Right(()))

      assert(errorProcessor.errors === forwardedErrors)

    }

  }

}

object KafkaConsumerProcessorSpec {

  val FixedError = new RuntimeException

  val FixedRecord = "{ \"wli\" : {} }"

  val TestConfig = KafkaConsumerConfig(List.empty, "test-group", "test-topic", KafkaPollConfig(101, 113, None), 1, 1, None)

  def newMockConsumer(cfg: KafkaConsumerConfig = TestConfig): MockConsumer[String, String] = {

    val consumer = new MockConsumer[String, String](OffsetResetStrategy.LATEST)

    consumer.subscribe(List(cfg.topic).asJavaCollection)

    val tp = new TopicPartition(TestConfig.topic, 0)

    consumer.rebalance(List(tp).asJavaCollection)

    consumer.updateEndOffsets(Map(tp -> new lang.Long(0L)).asJava)

    consumer
  }

  def addRecord(consumer: MockConsumer[String, String], topic: String, record: String, partition: Int = 0): Unit = {
    val offsets = consumer.endOffsets(List(new TopicPartition(topic, partition)).asJavaCollection)
    consumer.addRecord(new ConsumerRecord[String, String](topic, partition, offsets.get(topic) + 1, null, record))
  }

  class AggregateErrorProcessor[F[_]: Monad](var errors: List[ConsumingError] = List.empty) extends ErrorProcessor[F] {
    def processError(error: ConsumingError): F[Option[ConsumingError]] = {
      errors = error :: errors
      Monad[F].pure(None)
    }
  }

}
