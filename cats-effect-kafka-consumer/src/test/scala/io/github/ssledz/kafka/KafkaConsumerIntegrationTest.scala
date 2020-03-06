package io.github.ssledz.kafka

import java.util.Properties

import cats.effect.{Blocker, IO}
import cats.implicits._
import io.github.ssledz.kafka.Config.{KafkaConsumerConfig, KafkaPollConfig}
import io.github.ssledz.kafka.ThreadResources.KafkaBlocker
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.scalatest.FlatSpec
import org.scalatest.concurrent.Futures

import scala.concurrent.duration._

class KafkaConsumerIntegrationTest extends FlatSpec with Futures with KafkaDocker with KafkaConsumerProcessor {

  implicit val kafkaBlocker = KafkaBlocker(Blocker.liftExecutionContext(concurrent.ExecutionContext.global))
  implicit val cs = IO.contextShift(concurrent.ExecutionContext.global)

  implicit val errorHandler = new LogOnlyErrorHandler[IO]

  val testTopic = "test-topic"

  lazy val cfg = Config(KafkaConsumerConfig(brokers = List(s"localhost:$kafkaPort"), groupId = "test-group", topic = testTopic, poll = KafkaPollConfig(100)))

  it should "test" in {

    val consumer = ThreadResources.kafka.use { implicit kafkaBlocker =>
      KafkaConsumerProcessor.kafka(cfg.consumer).use { consumer =>
        pollLoop(cfg.consumer, consumer) { record =>
          IO(println(record))
        }.foreverM *> IO.unit
      }
    }

    val task = for {
      c <- consumer.start
      p <- producerJob(List("record 1", "record 2")).start
      _ <- c.join *> p.join
    } yield ()

    task.unsafeRunTimed(1000.millisecond)

  }

  def producerJob(records: List[String]): IO[Unit] = IO {
    val ps = new Properties
    val producer = new KafkaProducer[String, String](ps)
    records.foreach(r => producer.send(new ProducerRecord[String, String](testTopic, r)))
  }

}
