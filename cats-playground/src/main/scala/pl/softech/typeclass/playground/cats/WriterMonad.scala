package pl.softech.typeclass.playground.cats

import cats.data.Writer
import cats.implicits._


object MyWriterMonad {

  type Logged[A] = Writer[Vector[String], A]

  case class Contact(firstName: String, secondName: String, age: Int)

  def filterByFirstName(contact: Option[Contact], firstName: String): Logged[Option[Contact]] = contact match {
    case Some(c) => {
      if (c.firstName == firstName) contact.pure[Logged]
      else Writer(Vector(s"$contact filtered out because firstName != $firstName"), None)
    }
    case _ => contact.pure[Logged]
  }

  def filterByAgeGt(contact: Option[Contact], age: Int): Logged[Option[Contact]] =
    contact.pure[Logged]
      .flatMap { _ =>
        if (contact.exists(c => c.age >= age))
          contact.pure[Logged]
        else Writer(Vector(s"$contact filtered out beacuse age < $age"), None)
      }


  def filterContacts(contacts: Seq[Contact]): Logged[Seq[Contact]] = {
    val res = for {
      contact <- contacts
    } yield filterContact(contact)

    val zero = Seq.empty[Contact].pure[Logged]

    val res2 = res.foldLeft(zero)(
      (acc, el) => {
        val (accM, accSeq) = acc.run
        val (message, contact) = el.run
        (contact.map(Seq(_)).getOrElse(Seq.empty) ++ accSeq).writer(accM ++ message)
      }
    )
    res2
  }

  def filterContact(contact: Contact): Logged[Option[Contact]] = {

    for {
      c <- Option(contact).pure[Logged]
      r1 <- filterByFirstName(c, "Adam")
      r2 <- filterByAgeGt(r1, 12)
    } yield r2

  }

}

object MyWriterTest extends App {

  import MyWriterMonad._

  val contacts = Seq(
    Contact("Adam", "Warka", 16),
    Contact("Tom", "Warka", 12),
    Contact("Adam", "Marek", 14),
    Contact("Adam", "Buk", 11)
  )

  //  println(filterContacts(contacts))
  println(filterContacts(contacts))
  //  println(filterContact(contacts(0)))
  //  println(filterContact(contacts(1)))

}
