package pl.softech.typeclass.playground.state

import cats.data._


object StateExample extends App {

  case class Foo(x: Double)

  def bar(y: Double): State[Foo, Double] = for {
    foo <- State.get[Foo]
    _ <- State.set(Foo(foo.x + y))

  } yield foo.x / (foo.x + y)

  val xs: List[State[Foo, Double]] = List(1.0, 2.0, 3.0, 4.0).map(bar)

  def sequence(xs: List[State[Foo, Double]]): State[Foo, List[Double]] =
    xs.foldLeft(State.pure[Foo, List[Double]](List.empty[Double])) { (acc, x) =>
      for {
        xs <- acc
        xx <- x
      } yield xx :: xs
    }

  val s = sequence(xs)
  val ss = s.map(_.head)

  println(s.run(Foo(0)).value)
  println(ss.run(Foo(0)).value)

  println(ss.flatMap(_ => bar(3)).run(Foo(0)).value)

}
