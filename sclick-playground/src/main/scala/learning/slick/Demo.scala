package learning.slick

import learning.slick.comp.{H2MemDBComponent, MySqlDBComponent}
import learning.slick.domain.{Message, MessageRepository}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Demo extends App {

  mysqlDemo()
  h2MemDemo()

  def mysqlDemo(): Unit = {

    println("mysqlDemo")

    val init = for {
      _ <- MySqlMessageRepository.createSchemaIfNotExists
      _ <- MySqlMessageRepository.create(Message("tom", "Hello World"))
    } yield ()

    init.get

    val update = for {
      message <- MySqlMessageRepository.findById(1)
      _ = message.map(m => MySqlMessageRepository.update(m.copy(sender = "xyz"))).getOrElse(Future.successful(()))
    } yield ()

    update.get

    MySqlMessageRepository.findAll.get.foreach(println)

    println("findBySender(Some(\"xyz\"))")
    MySqlMessageRepository.findBySender(Some("xyz")).get.foreach(println)

  }

  def h2MemDemo(): Unit = {

    println("h2MemDemo")

    val init = for {
      _ <- H2MemMessageRepository.createSchemaIfNotExists
      _ <- H2MemMessageRepository.create(Message("tom", "Hello World"))
    } yield ()

    init.get

    H2MemMessageRepository.findAll.get.foreach(println)

  }


}


object MySqlMessageRepository extends MessageRepository with MySqlDBComponent

object H2MemMessageRepository extends MessageRepository with H2MemDBComponent