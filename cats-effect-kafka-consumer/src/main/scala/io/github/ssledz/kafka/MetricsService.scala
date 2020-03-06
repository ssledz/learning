package io.github.ssledz.kafka

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import cats.effect.{Resource, Sync}
import cats.implicits._
import com.codahale.metrics.graphite.{Graphite, GraphiteReporter}
import com.codahale.metrics.jvm.{GarbageCollectorMetricSet, MemoryUsageGaugeSet}
import com.codahale.metrics.{MetricFilter, MetricRegistry, ScheduledReporter, Slf4jReporter}
import com.typesafe.scalalogging.LazyLogging
import io.github.ssledz.kafka.Config.MetricsConfig
import io.github.ssledz.kafka.Config.MetricsConfig.GraphiteConfig
import org.slf4j.LoggerFactory

trait MetricsService[F[_]] {

  def measureAndLog[A](timerName: String)(op: => A): A

  def measureAndLogF[A](timerName: String)(fa: F[A]): F[A]

  def meter(name: String, n: Long = 1): Unit

  def close(): Unit

}

object MetricsService extends LazyLogging {

  def Nop[F[_]]: MetricsService[F] = new MetricsService[F] {
    def measureAndLog[A](timerName: String)(op: => A): A = op

    def meter(name: String, n: Long): Unit = ()

    def measureAndLogF[A](timerName: String)(fa: F[A]): F[A] = fa

    def close(): Unit = ()
  }

  def NopF[F[_]: Sync]: Resource[F, MetricsService[F]] = Resource.make(Sync[F].delay(Nop[F]))(_ => Sync[F].unit)

  def resource[F[_]: Sync](cfg: MetricsConfig): Resource[F, MetricsService[F]] = {

    val registry: MetricRegistry = new MetricRegistry

    val slf4j: F[ScheduledReporter] = Sync[F].delay(slf4jReporter(registry, cfg))

    val start: ScheduledReporter => F[ScheduledReporter] = r =>
      Sync[F].delay {
        r.start(cfg.schedule.toLong, TimeUnit.SECONDS)
        r
    }

    val reporter = cfg.graphite match {
      case Some(c) =>
        Sync[F]
          .delay(graphiteReporter(registry, c))
          .handleErrorWith { ex =>
            Sync[F].delay(logger.error("Error during initialization of metrics service, fallback to slf4j reporter", ex)) *> slf4j
          }
      case None => Sync[F].delay(logger.info("Using slf4j metrics reporter")) >> slf4j
    }

    registry.register("jvm.gc", new GarbageCollectorMetricSet)
    registry.register("jvm.mem", new MemoryUsageGaugeSet)

    Resource.make[F, MetricsService[F]](reporter >>= start >>= (r => Sync[F].delay(new DefaultMetricsService(registry, r))))(r => Sync[F].delay(r.close()))

  }

  private def graphiteReporter(registry: MetricRegistry, cfg: GraphiteConfig): ScheduledReporter = {
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

  private class DefaultMetricsService[F[_]: Sync](registry: MetricRegistry, reporter: ScheduledReporter) extends MetricsService[F] {

    def measureAndLog[A](timerName: String)(op: => A): A = registry.timer(timerName).time(() => op)

    def meter(name: String, n: Long): Unit = registry.meter(name).mark(n)

    def close(): Unit = reporter.close()

    def measureAndLogF[A](timerName: String)(fa: F[A]): F[A] =
      for {
        t <- Sync[F].delay(registry.timer(timerName).time())
        a <- fa
        _ <- Sync[F].delay(t.stop)
      } yield a

  }

}
