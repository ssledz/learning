package io.github.ssledz

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

class EntityManager {

  private val id = EntityManager.id.incrementAndGet()

  private val hugeData = new AtomicReference((1 to 1000000).toList)

  def close(): Unit = {
    hugeData.set(null)
  }

  def isOpen: Boolean = hugeData.get() != null

  def persist(entity: Object): Unit = {
    hugeData.updateAndGet(xs => xs ++ (1 to 100000).toList)
  }


  override def finalize(): Unit = println(s"em $id gc")

  override def toString: String = s"em:$id"
}

object EntityManager {
  val id = new AtomicInteger(1)
}