package learning.slick.domain


import learning.slick.{DbComponent, QueryLogging}

import scala.concurrent.{ExecutionContext, Future}

trait MessageRepository extends MessageTable with QueryLogging {
  this: DbComponent =>

  import driver.api._

  def create(message: Message): Future[Long] = db.run(messagesReturningId += message)

  def create(messages: Seq[Message]): Future[Seq[Long]] = db.run(messagesReturningId ++= messages)

  def createOrUpdate(message: Message)(implicit ec: ExecutionContext): Future[Message] = {
    if (message.id == -1) {
      db.run(traceQa(messagesReturningId into { (message, id) => message.copy(id = id) } += message))
    } else {
      update(message).map(_ => message)
    }
  }

  def createMessageIfNotExists(message: Message)(implicit ec: ExecutionContext): Future[Message] = {
    val data = Query((message.sender, message.content))
    val exists = messages.filter(m => m.sender === message.sender && m.content === message.content).exists
    val selectExpr = data.filterNot(_ => exists)
    db.run(traceQa(messages.map(m => m.sender -> m.content).forceInsertQuery(selectExpr))).map(id => message.copy(id = id))
  }

  def findAll: Future[List[Message]] = db.run(messages.to[List].result)

  def findById(id: Long): Future[Option[Message]] = db.run(traceQa(messages.filter(_.id === id).result).headOption)

  def findBySender(sender: Option[String]): Future[List[Message]] = db.run(messages.filterOpt(sender)(_.sender === _).to[List].result)

  def update(message: Message): Future[Int] = db.run(traceQa(messages.filter(_.id === message.id).update(message)))

  def deleteAll: Future[Int] = db.run(messages.delete)

  def updateSender(old2new: (String, String)): Future[Int] = old2new match {
    case (old, newOne) => db.run(traceQa(messages.filter(_.sender === old).map(_.sender).update(newOne)))
  }

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