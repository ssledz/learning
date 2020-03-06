package io.github.ssledz.kafka

import java.util.Properties

import cats.effect.{Blocker, IO}
import cats.implicits._
import com.whisk.docker.impl.dockerjava.DockerKitDockerJava
import io.github.ssledz.kafka.Config.{KafkaConsumerConfig, KafkaPollConfig}
import io.github.ssledz.kafka.ThreadResources.KafkaBlocker
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerConfig, ProducerRecord}
import org.scalatest.FlatSpec
import org.scalatest.concurrent.Futures

import scala.concurrent.duration._

class KafkaConsumerIntegrationTest extends FlatSpec with Futures with KafkaDocker with DockerKitDockerJava with KafkaConsumerProcessor {

  implicit val kafkaBlocker = KafkaBlocker(Blocker.liftExecutionContext(concurrent.ExecutionContext.global))
  implicit val cs = IO.contextShift(concurrent.ExecutionContext.global)

  implicit val errorHandler = new LogOnlyErrorHandler[IO]

  lazy val broker = s"localhost:$kafkaPort"

  lazy val cfg = Config(KafkaConsumerConfig(brokers = List(broker), groupId = "test-group", topic = testTopic, poll = KafkaPollConfig(100)))

  it should "test" in {

    val consumer = ThreadResources.kafka.use { implicit kafkaBlocker =>
      KafkaConsumerProcessor.kafka[IO](cfg.consumer).use { consumer =>
        pollLoop(cfg.consumer, consumer) { record =>
          IO(println(s"[${Thread.currentThread().getName}] consuming record: $record"))
        }.foreverM *> IO.unit
      }
    }

    val task = for {
      c <- (cs.shift *> consumer).start
      p <- (cs.shift *> producerJob(List("record 1", "record 2"))).start
      _ <- c.join *> p.join
    } yield ()

    task.unsafeRunTimed(10000.millisecond)

  }

  def producerJob(records: List[String]): IO[Unit] = IO {
    val ps = new Properties
    ps.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, broker)
    ps.put(ProducerConfig.ACKS_CONFIG, "0")
    ps.put(ProducerConfig.CLIENT_DNS_LOOKUP_CONFIG, "resolve_canonical_bootstrap_servers_only")
    ps.put(ProducerConfig.CLIENT_ID_CONFIG, "kafka-producer")
    ps.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringSerializer")
    ps.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringSerializer")
    val producer = new KafkaProducer[String, String](ps)
    Thread.sleep(3000)
    println("Producing....")
    records.foreach(r => producer.send(new ProducerRecord[String, String](testTopic, r)))
  }

}
