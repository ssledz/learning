package io.github.ssledz

class JpaService {

  private val emStore: ThreadLocal[EntityManager] = new ThreadLocal[EntityManager] {
    override def finalize(): Unit = {
      println("ems gc")
    }
  }

  def em: EntityManager = {
    var em = emStore.get()
    if (em == null || !em.isOpen) {
      em = new EntityManager
      emStore.set(em)
    }
    em
  }

  def closeEm(): Unit = {
    val em = emStore.get()
    if (em != null) {
      em.close()
    }
    emStore.remove()
  }

}
