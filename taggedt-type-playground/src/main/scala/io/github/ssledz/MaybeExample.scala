package io.github.ssledz
import io.github.ssledz.Maybe._
import io.github.ssledz.Tagger.@@

object MaybeExample extends App {

  val str: String @@ Maybe = Maybe("aa")
  val empty: String @@ Maybe = Maybe.none[String]

  println(str)
  println(Maybe.isJust(str))
  println(Maybe.isJust(empty))
  println(Maybe.getOrElse(empty, "bzz"))

  val xx = Maybe.just("")
  val res: String = empty.getOrElse(xx)

  println(res)

  def plus(ma: Integer @@ Maybe, mb: Integer @@ Maybe): Integer @@ Maybe =
    for {
      a <- ma
      b <- mb
    } yield a + b

  println(plus(Maybe.just(1), Maybe.just(2)))
  println("just 1 + none = " + plus(Maybe.just(1), Maybe.none))
  println(Maybe.map(Maybe.none[String])(s => "prefix_" + s))
  println(Maybe.map(Maybe.just("Hello-World"))(s => "prefix_" + s))

}
