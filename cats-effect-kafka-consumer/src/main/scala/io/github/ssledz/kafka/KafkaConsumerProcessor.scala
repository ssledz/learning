package io.github.ssledz.kafka

import java.time.Duration
import java.util.Properties

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import io.github.ssledz.kafka.Config.KafkaConsumerConfig
import io.github.ssledz.kafka.KafkaConsumerProcessor._
import org.apache.kafka.clients.CommonClientConfigs
import org.apache.kafka.clients.consumer.ConsumerConfig._
import org.apache.kafka.clients.consumer.KafkaConsumer
import org.apache.kafka.common.config.SaslConfigs
import org.apache.kafka.common.security.auth.SecurityProtocol

import scala.jdk.CollectionConverters._

trait KafkaConsumerProcessor extends LazyLogging {

  def pollLoop(cfg: KafkaConsumerConfig, consumer: KafkaConsumer[String, String], kcs: ContextShift[IO])(
      f: String => IO[Either[ConsumingError, Unit]])(implicit C: ContextShift[IO], E: ConsumingErrorHandler): IO[Unit] = {

    val repeat: IO[Unit] = for {

      records <- IO {
        val records = consumer.poll(Duration.ofMillis(cfg.poll.interval)).asScala.toList
        logger.trace("poll.all ({})", records.size)
        records
      }

      result <- records.parTraverse { record =>
        f(record.value).handleErrorWith { error =>
          IO.pure(Left(UnknownError(record.value, error)))
        }
      }

      (errors, _) = result.partition(_.isLeft)
      _ <- E.handleErrors(errors.flatMap(_.swap.toSeq))
      _ <- kcs.shift

    } yield ()

    kcs.shift *> repeat.foreverM
  }

}

object KafkaConsumerProcessor {

  def createConsumer(cfg: KafkaConsumerConfig): KafkaConsumer[String, String] = {
    val c = new KafkaConsumer[String, String](props(cfg))
    c.subscribe(List(cfg.topic).asJava)
    c
  }

  private def props(cfg: KafkaConsumerConfig): Properties = {
    val ps = new Properties
    ps.put(BOOTSTRAP_SERVERS_CONFIG, cfg.brokers.mkString(","))
    ps.put(GROUP_ID_CONFIG, cfg.groupId)
    ps.put(KEY_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer")
    ps.put(VALUE_DESERIALIZER_CLASS_CONFIG, "org.apache.kafka.common.serialization.StringDeserializer")
    ps.put(MAX_PARTITION_FETCH_BYTES_CONFIG, cfg.maxPartitionFetchBytes)
    ps.put(FETCH_MAX_BYTES_CONFIG, cfg.fetchMaxBytes)

    cfg.jaasCredentials.foreach { cred =>
      ps.put(SaslConfigs.SASL_JAAS_CONFIG, cred)
      ps.put(SaslConfigs.SASL_MECHANISM, "PLAIN")
      ps.put(CommonClientConfigs.SECURITY_PROTOCOL_CONFIG, SecurityProtocol.SASL_PLAINTEXT.name)
    }
    ps.put(MAX_POLL_INTERVAL_MS_CONFIG, int2Integer(cfg.poll.maxInterval))
    cfg.poll.maxRecords.foreach(v => ps.put(MAX_POLL_RECORDS_CONFIG, int2Integer(v)))
    ps
  }

  sealed trait ConsumingError

  case class UnknownError(record: String, exception: Throwable) extends ConsumingError

}
