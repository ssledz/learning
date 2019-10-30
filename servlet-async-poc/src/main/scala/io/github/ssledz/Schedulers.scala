package io.github.ssledz

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, ThreadFactory}

import cats.effect.IO

import scala.concurrent.ExecutionContext

object Schedulers {

  private val blockingPool = Executors.newCachedThreadPool(new DaemonThreadFactory("blocking-scheduler"))

  private val numberOfCpus = Runtime.getRuntime.availableProcessors

  private val cpuBoundedPool = Executors.newFixedThreadPool(numberOfCpus, new DaemonThreadFactory("cpu-bounded-scheduler"))

  lazy val blocking = IO.contextShift(ExecutionContext.fromExecutor(blockingPool))

  lazy val cpu = IO.contextShift(ExecutionContext.fromExecutor(cpuBoundedPool))

  private class DaemonThreadFactory(name: String) extends ThreadFactory {

    private val poolNumber = new AtomicInteger(1)
    private val threadNumber = new AtomicInteger(1)

    private val namePrefix = name + "-" + poolNumber.getAndIncrement + "-thread-"

    val s: SecurityManager = System.getSecurityManager

    private val group = if (s != null) s.getThreadGroup else Thread.currentThread.getThreadGroup

    override def newThread(r: Runnable): Thread = {
      val t = new Thread(group, r, namePrefix + threadNumber.getAndIncrement, 0)
      t.setDaemon(true)
      t.setPriority(Thread.NORM_PRIORITY)
      t
    }

  }

}
