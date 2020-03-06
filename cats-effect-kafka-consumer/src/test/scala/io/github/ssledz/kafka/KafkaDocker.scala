package io.github.ssledz.kafka

import com.typesafe.scalalogging.LazyLogging
import com.whisk.docker._
import com.whisk.docker.scalatest.DockerTestKit
import org.scalatest.Suite
import org.scalatest.time.{Milliseconds, Seconds, Span}

import scala.language.implicitConversions

trait KafkaDocker extends DockerTestKit with DockerKit with LazyLogging { self: Suite =>

  implicit val pc: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(100, Milliseconds))

  private val kafkaContainerPort: Int = 9092

  private val zookeeperContainerPort: Int = 2181

  def kafkaContainerName: Option[String] = None

  def zookeeperContainerName: String = "kafka-zookeeper"

  private val zookeeper: DockerContainer = DockerContainer("wurstmeister/zookeeper", name = Some(zookeeperContainerName))
    .withPorts(zookeeperContainerPort -> Some(zookeeperContainerPort))

  private val kafkaContainer: DockerContainer = DockerContainer("wurstmeister/kafka:2.12-2.4.0", name = kafkaContainerName)
    .withPortMapping(kafkaContainerPort -> DockerPortMapping(None))
    .withEnv(
      s"KAFKA_ZOOKEEPER_CONNECT=$zookeeperContainerName:$zookeeperContainerPort",
      "KAFKA_CREATE_TOPICS=tagging-events:1:1",
      "HOSTNAME_COMMAND=route -n | awk '/UG[ ]/{print $2}'"
    )
    .withVolumes(Seq(VolumeMapping("/var/run/docker.sock", "/var/run/docker.sock")))
    .withLinks(ContainerLink(zookeeper, zookeeperContainerName))
    .withLogLineReceiver(LogLineReceiver(true, line => logger.debug(line)))

  def kafkaPort: Int = kafkaContainer.getPorts().map(_.apply(kafkaContainerPort)).futureValue

  override def dockerContainers: List[DockerContainer] = zookeeper :: kafkaContainer :: super.dockerContainers

}
