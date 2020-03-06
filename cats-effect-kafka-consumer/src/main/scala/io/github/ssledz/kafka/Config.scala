package io.github.ssledz.kafka

import java.net.InetAddress

import cats.Show
import io.github.ssledz.kafka.Config.MetricsConfig.GraphiteConfig
import io.github.ssledz.kafka.Config.{KafkaConsumerConfig, MetricsConfig}
import org.apache.kafka.clients.consumer.ConsumerConfig
import pureconfig.ConfigReader.Result
import pureconfig.{ConfigReader, ConfigSource}

case class Config(consumer: KafkaConsumerConfig, metrics: Option[MetricsConfig] = None)

object Config {

  import pureconfig.generic.auto._

  implicit val graphiteReaderOpt: ConfigReader[Option[GraphiteConfig]] = ConfigReader.fromFunction { cfg =>
    ConfigReader[GraphiteConfig].from(cfg) match {
      case Left(_) => Right(None)
      case Right(value) => Right(Some(value))
    }
  }

  def load: Result[Config] = ConfigSource.default.load[Config]

  implicit val showInstance: Show[Config] = c => s"""
       | consumer.brokers                     : ${c.consumer.brokers}
       | consumer.groupId                     : ${c.consumer.groupId}
       | consumer.topic                       : ${c.consumer.topic}
       | consumer.poll.maxInterval            : ${c.consumer.poll.maxInterval}
       | consumer.poll.interval               : ${c.consumer.poll.interval}
       | consumer.poll.maxRecords             : ${c.consumer.poll.maxRecords.getOrElse("none")}
       | consumer.poll.maxPartitionFetchBytes : ${c.consumer.maxPartitionFetchBytes}
       | consumer.poll.fetchMaxBytes          : ${c.consumer.fetchMaxBytes}
       | consumer.jaasCredentials             : ${c.consumer.jaasCredentials.getOrElse("none")}
       | metrics.schedule                     : ${c.metrics.map(_.schedule).getOrElse("none")}
       | metrics.slf4jLoggerName              : ${c.metrics.map(_.slf4jLoggerName).getOrElse("none")}
       | metrics.graphite.host                : ${c.metrics.flatMap(_.graphite.map(_.host)).getOrElse("none")}
       | metrics.graphite.port                : ${c.metrics.flatMap(_.graphite.map(_.port)).getOrElse("none")}
       | metrics.graphite.prefix              : ${c.metrics.flatMap(_.graphite.map(_.prefix)).getOrElse("none")}
       |""".stripMargin

  case class KafkaConsumerConfig(
      brokers: List[String],
      groupId: String,
      topic: String,
      poll: KafkaPollConfig,
      maxPartitionFetchBytes: Int = ConsumerConfig.DEFAULT_MAX_PARTITION_FETCH_BYTES,
      fetchMaxBytes: Int = ConsumerConfig.DEFAULT_FETCH_MAX_BYTES,
      jaasCredentials: Option[String] = None)

  case class KafkaPollConfig(interval: Long, maxInterval: Int = 3000, maxRecords: Option[Int] = None) {
    assert(interval < maxInterval)
  }

  case class MetricsConfig(graphite: Option[GraphiteConfig], slf4jLoggerName: String, schedule: Int = 60)

  object MetricsConfig {

    case class GraphiteConfig(port: Int, host: String, prefix: String) {
      def resolvePrefix: String = prefix.replace("[ip]", InetAddress.getLocalHost.getHostName)
    }

  }

}
