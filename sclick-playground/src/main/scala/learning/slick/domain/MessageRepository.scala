package learning.slick.domain


import learning.slick.DbComponent

import scala.concurrent.Future

trait MessageRepository extends MessageTable {
  this: DbComponent =>

  import driver.api._

  def create(message: Message): Future[Long] = db.run((messages returning messages.map(_.id)) += message)

  def findAll: Future[List[Message]] = db.run(messages.to[List].result)

  def findById(id: Long): Future[Option[Message]] = db.run(messages.filter(_.id === id).result.headOption)

  def update(message : Message) : Future[Int] = db.run(messages.filter(_.id === message.id).update(message))

}

private[domain] trait MessageTable {
  this: DbComponent =>

  import driver.api._

  private[MessageTable] final class MessageTable(tag: Tag) extends Table[Message](tag, "message") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def sender = column[String]("sender")

    def content = column[String]("content")

    def * = (sender, content, id).mapTo[Message]
  }

  protected val messages = TableQuery[MessageTable]


  def createSchemaIfNotExists: Future[Unit] = db.run(messages.schema.createIfNotExists)

}


final case class Message(sender: String,
                         content: String,
                         id: Long = 0L)