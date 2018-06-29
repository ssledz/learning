package pl.softech.typeclass.playground.cats

import cats.Show
import cats.implicits._


object ShowInstances {

  implicit val catInstance: Show[Cat] = Show.show(cat => {
    val name = Show[String].show(cat.name)
    val color = Show[String].show(cat.color)
    val age = Show[Int].show(cat.age)
    s"NAME is $name AGE $age COLOR $color"
  })

}


object TestShow extends App {

  import ShowInstances._

  println(Cat("Tom", 11, "black").show)


}
