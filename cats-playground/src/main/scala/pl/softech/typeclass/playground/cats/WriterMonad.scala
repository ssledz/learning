package pl.softech.typeclass.playground.cats

import cats.Monad
import cats.data.{State, Writer}
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

  def filterByAgeGt(contact: Option[Contact], age: Int): Logged[Option[Contact]] = contact match {
    case Some(_) => contact.pure[Logged]
      .flatMap { _ =>
        if (contact.exists(c => c.age >= age))
          contact.pure[Logged]
        else Writer(Vector(s"$contact filtered out beacuse age < $age"), None)
      }
    case _ => contact.pure[Logged]
  }

  def sequence[F[_], A](in: Seq[F[A]])(implicit m: Monad[F]): F[Seq[A]] = {
    val zero = m.pure(Seq.empty[A])
    in.foldLeft(zero)(
      (acc: F[Seq[A]], el: F[A]) => {
        m.flatMap(acc)(l => {
          m.map(el)(e => l :+ e)
        })
      }
    )
  }

  def filterContacts(contacts: Seq[Contact]): Logged[Seq[Contact]] = {
    val res = for {
      contact <- contacts
    } yield filterContact(contact)

    sequence(res).map(_.flatten)
  }

  def filterContacts2(contacts: Seq[Contact]): Logged[Seq[Contact]] = {
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

  def filterContact2(contact: Contact): Logged[Option[Contact]] = {

    for {
      c <- Option(contact).pure[Logged]
      r1 <- filterByFirstName(c, "Adam")
      r2 <- filterByAgeGt(r1, 12)
    } yield r2

  }

  def filterContact(contact: Contact): Logged[Option[Contact]] = {
    import State._
    type LoggedSt = Logged[Option[Contact]]
    val filter: State[LoggedSt, LoggedSt] = for {
      c <- get[LoggedSt]
      _ <- modify[LoggedSt](_.flatMap(c => filterByFirstName(c, "Adam")))
      _ <- modify[LoggedSt](_.flatMap(c => filterByAgeGt(c, 12)))
    } yield c

    filter.runS(Option(contact).pure[Logged]).value

  }

}

object MyGenericLoggedFiltering {

  type Logged[A] = Writer[Vector[String], A]

  def filter[A](f: A => Logged[Option[A]])(xs: Seq[A]): Logged[Seq[A]] = {
    def sequence[A](xs: Seq[Logged[A]]): Logged[Seq[A]] = {
      val zero = Seq.empty[A].pure[Logged]
      xs.foldLeft(zero)(
        (acc: Logged[Seq[A]], el: Logged[A]) => acc.flatMap(l => el.map(e => l :+ e))
      )
    }

    sequence(xs.map(f)).map(_.flatten)
  }

  def aggregate[A](xs: List[Option[A] => Logged[Option[A]]]): A => Logged[Option[A]] =
    sub =>
      xs.foldLeft(Option(sub).pure[Logged]) {
        (acc: Logged[Option[A]], el) => acc.flatMap(el)
      }
}

object MyWriterTest extends App {

  import MyGenericLoggedFiltering._
  import MyWriterMonad._

  val contacts = Seq(
    Contact("Adam", "Warka", 16),
    Contact("Tom", "Warka", 12),
    Contact("Adam", "Marek", 14),
    Contact("Adam", "Buk", 11)
  )

  //  println(filterContacts(contacts))
  //  println(filterContacts(contacts))
  //  println(filterContact(contacts(0)))
  //  println(filterContact(contacts(1)))

  //  println(sequence(List(Option(1), Option(2))))

  def filteringContacts = filter(filterContact) _

  val contactPredicate: Contact => MyGenericLoggedFiltering.Logged[Option[Contact]] =
    aggregate(List(
      filterByFirstName(_, "Adam"),
      filterByAgeGt(_, 12)
    ))

  def filteringContacts2 = filter(contactPredicate) _

  println(filteringContacts(contacts))
  println(filteringContacts2(contacts))

  val ops = Seq((_: Int) + 1, (_: Int) * 2)
  val x = ops.reduce(_ andThen _)
  //  println(x(2))

}
