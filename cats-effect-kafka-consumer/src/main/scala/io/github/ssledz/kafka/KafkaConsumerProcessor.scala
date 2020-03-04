package io.github.ssledz.kafka

import java.time.Duration
import java.util.Properties

import cats.Parallel
import cats.effect.{ContextShift, Resource, Sync}
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import io.github.ssledz.kafka.Config.KafkaConsumerConfig
import io.github.ssledz.kafka.KafkaConsumerProcessor.ConsumingError
import io.github.ssledz.kafka.ThreadResources.KafkaBlocker
import org.apache.kafka.clients.CommonClientConfigs
import org.apache.kafka.clients.consumer.ConsumerConfig._
import org.apache.kafka.clients.consumer.{Consumer, KafkaConsumer}
import org.apache.kafka.common.config.SaslConfigs
import org.apache.kafka.common.security.auth.SecurityProtocol

import scala.jdk.CollectionConverters._

trait KafkaConsumerProcessor extends LazyLogging {

  def pollLoop[F[_]: Sync: ContextShift: Parallel](cfg: KafkaConsumerConfig, consumer: Consumer[String, String])(
      f: String => F[Unit])(implicit blocker: KafkaBlocker, E: ConsumingErrorHandler[F]): F[Unit] =
    for {

      records <- blocker.underlying.blockOn(Sync[F].delay(consumer.poll(Duration.ofMillis(cfg.poll.interval)).asScala.toList))

      result <- records.parTraverse { record =>
        f(record.value).attempt.map {
          case Left(err) => Left(ConsumingError(record.value, err))
          case _ => Right(())
        }
      }

      (errors, _) = result.partition(_.isLeft)
      _ <- E.handleErrors(errors.flatMap(_.swap.toSeq))

    } yield ()

}

object KafkaConsumerProcessor {

  def kafka[F[_]: Sync: ContextShift](cfg: KafkaConsumerConfig)(implicit blocker: KafkaBlocker): Resource[F, KafkaConsumer[String, String]] =
    Resource.make(Sync[F].delay(createConsumer(cfg)))(c => blocker.underlying.blockOn(Sync[F].delay(c.close()))(ContextShift[F]))

  private def createConsumer(cfg: KafkaConsumerConfig): KafkaConsumer[String, String] = {
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

  case class ConsumingError(record: String, exception: Throwable)

}
