package pl.softech.typeclass.playground.printable

import pl.softech.typeclass.playground.cats.Cat

trait Printable[A] {

  def format(value: A): String

}

object PrintableInstances {

  implicit val stringInstance: Printable[String] = (value: String) => value
  implicit val intInstance: Printable[Int] = (value: Int) => value.toString

  implicit val catInstance: Printable[Cat] = (value: Cat) => {
    val name = Printable.format(value.name)
    val age = Printable.format(value.age)
    val color = Printable.format(value.color)
    s"NAME is $name AGE $age COLOR $color"
  }

}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {

    def println(implicit p: Printable[A]) = Printable.print(value)

    def format(implicit p: Printable[A]) = Printable.format(value)

  }

}

object Printable {

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))


}

object TestPrintable extends App {


  import PrintableInstances._
  import PrintableSyntax._

  Printable.print("Ala ma kota")
  Printable.print(1111)
  111.println
  "Foo".println
  Cat("Tom", 11, "black").println

}
