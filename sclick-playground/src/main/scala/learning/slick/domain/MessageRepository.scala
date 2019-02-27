package learning.slick.domain


import learning.slick.{DbComponent, QueryLogging}

import scala.concurrent.{ExecutionContext, Future}

trait MessageRepository extends MessageTable with QueryLogging {
  this: DbComponent =>

  import driver.api._

  def create(message: Message): Future[Long] = db.run(messagesReturningId += message)

  def createOrUpdate(message: Message)(implicit ec: ExecutionContext): Future[Message] = {
    if (message.id == -1) {
      db.run(traceQa(messagesReturningId into { (message, id) => message.copy(id = id) } += message))
    } else {
      update(message).map(_ => message)
    }
  }

  def findAll: Future[List[Message]] = db.run(messages.to[List].result)

  def findById(id: Long): Future[Option[Message]] = db.run(traceQa(messages.filter(_.id === id).result).headOption)

  def findBySender(sender: Option[String]): Future[List[Message]] = db.run(messages.filterOpt(sender)(_.sender === _).to[List].result)

  def update(message: Message): Future[Int] = db.run(traceQa(messages.filter(_.id === message.id).update(message)))

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

  protected def messagesReturningId = messages returning messages.map(_.id)


  def createSchemaIfNotExists: Future[Unit] = db.run(messages.schema.createIfNotExists)

}


final case class Message(sender: String,
                         content: String,
                         id: Long = -1L)