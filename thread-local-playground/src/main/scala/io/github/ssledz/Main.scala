package io.github.ssledz

import java.util.concurrent.Executors
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import io.github.ssledz.domain.{JpaUserDao, User, UserDao}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object Main extends App {

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())

//  entityManagerInConcurrentEnv()
    transactorExample()

  def entityManagerInConcurrentEnv(): Unit = {
    val jpaService = new JpaService
    val userDao = new JpaUserDao(jpaService)
    val acc = new AtomicInteger(1)
    val start = System.currentTimeMillis()

    def duration: Long = (System.currentTimeMillis() - start) / 1000

    while (true) {
      println(s"${duration}s used memory: $usedMemory MB")
      val fs = (1 to 8).map(_ => Future(userDao.saveUser(User("user" + acc.incrementAndGet()))))
      Await.result(Future.sequence(fs), 10.second)
      Thread.sleep(1000)
      jpaService.closeEm()
    }

  }

  def transactorExample(): Unit = {
    val userDao = new UserDao

    val acc = new AtomicInteger(1)

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
//      val p = tr.get(threads.get()).map(_.toString).mkString("[", ",", "]")
//      println(p)
      Thread.sleep(100)
      tr.close()
    }
  }

  def usedMemory: Long = (Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory) / 1024 / 1024

}
