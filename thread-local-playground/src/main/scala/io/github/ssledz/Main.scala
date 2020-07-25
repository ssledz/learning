package io.github.ssledz

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Main extends App {

  class EntityManager {

    private val id = EntityManager.id.incrementAndGet()

    private var hugeData = (1 to 1000000).toList

    def close(): Unit = {
      hugeData = null
    }

    def persist(entity: Object): Unit = {
      //      println(s"persisting entity: $entity")
    }


    override def finalize(): Unit = println(s"em $id gc")

    override def toString: String = s"em:$id"
  }

  object EntityManager {
    val id = new AtomicInteger(1)
  }

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

  case class User(name: String)

  class UserDao {
    def saveUser(u: User)(implicit tr: Transactor): Unit = tr.em.persist(u)
  }

  import concurrent.ExecutionContext.Implicits.global

  val userDao = new UserDao

  val acc = new AtomicInteger(1)

  def usedMemory: Long = (Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory) / 1024 / 1024

  val start = System.currentTimeMillis()

  def duration: Long = (System.currentTimeMillis() - start) / 1000

  while (true) {
    println(s"${duration}s used memory: $usedMemory MB")
    implicit val tr = new Transactor
    val threads = new AtomicReference[List[Thread]](List.empty)
    val fs = (1 to 8).map(_ => Future {
      threads.updateAndGet(Thread.currentThread() :: _)
      userDao.saveUser(User("user" + acc.incrementAndGet()))
    })
    Await.result(Future.sequence(fs), 10.second)
    val p = tr.get(threads.get()).map(_.toString).mkString("[", ",", "]")
    println(p)
    tr.close()
    Thread.sleep(1000)
  }

}
