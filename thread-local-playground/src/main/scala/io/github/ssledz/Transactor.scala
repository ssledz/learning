package io.github.ssledz

import java.util.concurrent.atomic.AtomicReference

class Transactor {

  private val emStore: ThreadLocal[EntityManager] = new ThreadLocal[EntityManager] {
    override def finalize(): Unit = {
      println("ems gc")
    }
  }

  private val ems: AtomicReference[Map[EntityManager, Thread]] = new AtomicReference(Map.empty)

  private def register(em: EntityManager): EntityManager = {
    ems.updateAndGet((xs: Map[EntityManager, Thread]) => xs + (em -> Thread.currentThread()))
    em
  }

  private def getOrNew: EntityManager = {
    var em = emStore.get()
    if (em == null) {
      em = new EntityManager
      emStore.set(em)
    }
    em
  }

  import ThreadLocalSyntax._

  private def closeEm(em: EntityManager, thread: Thread): Unit = {

    em.close()
    emStore.removeT(thread)
  }

  def close(): Unit = ems.get().foreach { case (em, thread) => closeEm(em, thread) }

  def em: EntityManager = register(getOrNew)

  def get(ths: List[Thread]): List[EntityManager] = ths.flatMap(emStore.getT)

}
