package pl.softech.typeclass.playground.cats

import cats._
import cats.implicits._

object EqInstances {

  implicit val catInstance: Eq[Cat] = Eq.fromUniversalEquals

}

object TestEq extends App {

  import cats.instances.int._ // for Eq
  val eqInt = Eq[Int]

  println(eqInt.eqv(123, 123))

  println(List(1, 2, 3).map(Option(_)).filter(item => item == 1))
  println(List(1, 2, 3).map(Option(_)).filter(item => item === Option(1)))

  val cat1 = Cat("Tom", 11, "black")
  val cat2 = cat1.copy(age = 12)
  val cat3 = cat1.copy()

  import EqInstances._

  println(s"cat1 === cat2 : ${cat1 === cat2}")
  println(cat1 === cat3)
  println(Option(cat1) === Option(cat3))
  println(Option(cat1) == Option(cat3))
  println(Option(cat1) == cat3)
//  println(Option(cat1) === cat3)


}
