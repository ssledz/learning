package pl.softech.typeclass.playground.cats

import cats.implicits._
import cats.{Eq, Monoid}


case class BooleanOr(value: Boolean)

object MonoidInstances {

  implicit val booleanOrMonoid: Monoid[BooleanOr] = new Monoid[BooleanOr] {
    override def empty: BooleanOr = BooleanOr(true)

    override def combine(x: BooleanOr, y: BooleanOr): BooleanOr = (x, y) match {
      case (BooleanOr(a), BooleanOr(b)) => BooleanOr(a || b)
    }
  }

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

}

object EqInstances {
  implicit val eqBooleanOr: Eq[BooleanOr] = (x: BooleanOr, y: BooleanOr) => x == y
}


case class Order(totalCost: Double, quantity: Double)

object MonoidTest extends App {

  import EqInstances._
  import MonoidInstances._

  val x1 = Monoid[BooleanOr].empty
  val x2 = BooleanOr(false)

  println(Monoid[BooleanOr].combine(x1, x2))
  println(x1.isEmpty)
  println(x2.isEmpty)

  println(x1 |+| x2)
  println(Option(x1) |+| Option(x2))

  val intResult = 1 |+| 2 |+| Monoid[Int].empty
  println(intResult)

  def add[A](items: List[A])(implicit m: Monoid[A]): A = {
    items.foldLeft(m.empty)(_ |+| _)
  }

  def add2[A: Monoid](items: List[A]): A = {
    items.foldLeft(Monoid[A].empty)(_ |+| _)
  }

  println(add(List(1, 2, 4, 5)))
  println(add(List(1, 2, 4, 5).map(Option(_))))
  println(add(List(Order(200, 1), Order(100, 3)).map(Option(_))))

}
