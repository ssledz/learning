package io.github.ssledz

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Main extends App {

  class EntityManager {

    private val id = EntityManager.id.incrementAndGet()

    private val hugeData = (1 to 1000000).toList

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

    val ems: ThreadLocal[EntityManager] = new ThreadLocal[EntityManager] {
      override def finalize(): Unit = {
        println("ems gc")
      }
    }

    def em: EntityManager = {
      var em = ems.get()
      if (em == null) {
        em = new EntityManager
        ems.set(em)
      }
      em
    }
  }

  case class User(name: String)

  class UserDao {
    def saveUser(u: User)(implicit tr: Transactor): Unit = tr.em.persist(u)
  }

  import ThreadLocalSyntax._

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
    Future.sequence(fs)
    Await.result(Future.sequence(fs), 10.second)
    val p = threads.get().map(tr.ems.getT).map(_.toString).mkString("[", ",", "]")
    println(p)
//    threads.get().foreach(tr.ems.removeT)
    Thread.sleep(1000)
  }

}
