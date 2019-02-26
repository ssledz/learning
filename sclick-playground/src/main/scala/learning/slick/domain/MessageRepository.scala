package learning.slick.domain

import learning.slick.DbComponent

trait MessageRepository extends MessageTable {
  this: DbComponent =>

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

  val schemaCreateAction: DBIOAction[Unit, NoStream, Effect.Schema] = messages.schema.create

}


final case class Message(
                          sender:
                          String,
                          content: String,
                          id:
                          Long = 0L)