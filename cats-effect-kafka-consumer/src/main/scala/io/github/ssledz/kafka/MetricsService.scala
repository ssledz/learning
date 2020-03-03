package io.github.ssledz.kafka

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import cats.effect.{IO, Resource}
import cats.implicits._
import com.codahale.metrics.graphite.{Graphite, GraphiteReporter}
import com.codahale.metrics.jvm.{GarbageCollectorMetricSet, MemoryUsageGaugeSet}
import com.codahale.metrics.{MetricFilter, MetricRegistry, ScheduledReporter, Slf4jReporter}
import io.github.ssledz.kafka.Config.MetricsConfig
import io.github.ssledz.kafka.Config.MetricsConfig.GraphiteConfig
import org.slf4j.LoggerFactory

trait MetricsService {

  def measureAndLog[A](timerName: String)(op: => A): A

  def measureAndLogF[A](timerName: String)(fa: IO[A]): IO[A]

  def meter(name: String, n: Long = 1): Unit

  def close(): Unit

}

object MetricsService {

  private lazy val logger = LoggerFactory.getLogger(classOf[MetricsService])

  val Nop: MetricsService = new MetricsService {
    def measureAndLog[A](timerName: String)(op: => A): A = op

    def meter(name: String, n: Long): Unit = ()

    def measureAndLogF[A](timerName: String)(fa: IO[A]): IO[A] = fa

    def close(): Unit = ()
  }

  val NopIO: Resource[IO, MetricsService] = Resource.make(IO(Nop))(_ => IO.unit)

  def resource(cfg: MetricsConfig): Resource[IO, MetricsService] = {

    val registry: MetricRegistry = new MetricRegistry

    val slf4j = IO(slf4jReporter(registry, cfg))

    val start: ScheduledReporter => IO[ScheduledReporter] = r =>
      IO {
        r.start(cfg.schedule.toLong, TimeUnit.SECONDS)
        r
    }

    val reporter = cfg.graphite match {
      case Some(c) =>
        IO(graphiteReporter(registry, c))
          .handleErrorWith { ex =>
            IO(logger.error("Error during initialization of metrics service, fallback to slf4j reporter", ex)) >> slf4j
          }
      case None => IO(logger.info("Using slf4j metrics reporter")) >> slf4j
    }

    registry.register("jvm.gc", new GarbageCollectorMetricSet)
    registry.register("jvm.mem", new MemoryUsageGaugeSet)

    Resource.make[IO, MetricsService](reporter >>= start >>= (r => IO(new DefaultMetricsService(registry, r))))(r => IO(r.close()))

  }

  private def graphiteReporter(registry: MetricRegistry, cfg: GraphiteConfig): GraphiteReporter = {
    val graphite = new Graphite(new InetSocketAddress(cfg.host, cfg.port))
    GraphiteReporter
      .forRegistry(registry)
      .prefixedWith(cfg.resolvePrefix)
      .convertRatesTo(TimeUnit.SECONDS)
      .convertDurationsTo(TimeUnit.MILLISECONDS)
      .filter(MetricFilter.ALL)
      .build(graphite)
  }

  private def slf4jReporter(registry: MetricRegistry, cfg: MetricsConfig): Slf4jReporter =
    Slf4jReporter
      .forRegistry(registry)
      .outputTo(LoggerFactory.getLogger(cfg.slf4jLoggerName))
      .convertRatesTo(TimeUnit.SECONDS)
      .convertDurationsTo(TimeUnit.MILLISECONDS)
      .build

  private class DefaultMetricsService(registry: MetricRegistry, reporter: ScheduledReporter) extends MetricsService {

    def measureAndLog[A](timerName: String)(op: => A): A = registry.timer(timerName).time(() => op)

    def meter(name: String, n: Long): Unit = registry.meter(name).mark(n)

    def close(): Unit = reporter.close()

    def measureAndLogF[A](timerName: String)(fa: IO[A]): IO[A] =
      for {
        t <- IO(registry.timer(timerName).time())
        a <- fa
        _ <- IO(t.stop)
      } yield a

  }

}
