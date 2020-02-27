package io.github.ssledz.kafka

import java.util.concurrent.Executors.{newCachedThreadPool, newSingleThreadExecutor}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, ThreadFactory}

import cats.effect.{Blocker, ContextShift, IO, Resource}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object ThreadResources {

  def kafka: Resource[IO, (ExecutorService, ExecutionContextExecutor, ContextShift[IO])] =
    make(newSingleThreadExecutor(newThreadFactory("kafka-pool")))

  def blocking: Resource[IO, (ExecutorService, Blocker, ContextShift[IO])] =
    make(newCachedThreadPool(newThreadFactory("blocking-pool")))
      .map { case (es, ec, cs) => (es, Blocker.liftExecutionContext(ec), cs) }

  def workStealing: Resource[IO, (ExecutorService, ExecutionContextExecutor, ContextShift[IO])] =
    make(newCachedThreadPool(newThreadFactory("work-stealing")))

  private def make(es: => ExecutorService): Resource[IO, (ExecutorService, ExecutionContextExecutor, ContextShift[IO])] =
    Resource.make {
      val ec = ExecutionContext.fromExecutor(es)
      val cs: ContextShift[IO] = IO.contextShift(ec)
      IO((es, ec, cs))
    } { case (es, _, _) => IO(es.shutdown) }

  private def newThreadFactory(name: String): ThreadFactory = new ThreadFactory {
    val ctr = new AtomicInteger(0)

    def newThread(r: Runnable): Thread = {
      val back = new Thread(r)
      back.setName(s"$name-${ctr.getAndIncrement()}")
      back.setDaemon(true)
      back
    }
  }

}
