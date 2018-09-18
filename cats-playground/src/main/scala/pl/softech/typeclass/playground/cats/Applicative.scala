package pl.softech.typeclass.playground.cats

object MyApplicative {

}

object TestMyApplicative extends App {

  import cats._
  import cats.data._
  import cats.implicits._

  val hs = Functor[List].map(List(1, 2, 3, 4))({ (_: Int) * (_: Int) }.curried)
  println(Functor[List].map(hs) { _ (9) })

  println(1.some)

  val res1 = (3.some |@| 5.some) map { _ - _ }
  val res2 = (3.some, 5.some) mapN { _ - _ }
  println(res1)
  println(res2)

  println((none[Int], 5.some) mapN { _ - _ })

  val res3 = (List("ha", "heh", "hmm"), List("?", "!", ".")) mapN { _ + _ }
  println(res3)

  println(1.some <* 2.some)
  println(1.some *> 2.some)


  val res4 = Apply[Option].ap({{(_: Int) + 3}.some })(9.some)
  println(res4)

  val res5 = 2.some.map({(_: Int) * (_: Int)}.curried) <*> 3.some
  println(res5)


  println(1.some.product(2.some))
  println(List("a", "b", "c").product(List("d", "e")))
}


