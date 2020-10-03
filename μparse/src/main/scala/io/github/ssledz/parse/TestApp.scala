package io.github.ssledz.parse

import io.github.ssledz.parse.Parsers._

object TestApp extends App {

  val abc = string("abc")
  val defg = string("defg")

  println(run(abc)("abcdefg"))
  println(run(abc)("1abcdefg"))
  println(run(succeed(1))("abc"))

  println(run(abc ** defg)("abcdefg"))

  val p1 = scope("abc**abc**defg or abc**defg")(attempt(abc ** abc ** defg) or scope("abc**defg")(abc ** defg))

  println(run(p1)("abcdefg"))
  println(run(p1)("abcabcdefg"))
  println(run(p1)("abcabcabcdefg"))

  println(run(slice(many(attempt(char('a')) | char('b'))))("aaba"))
  println(run(slice(many(attempt(char('a')) | char('b'))))(""))
  println(run(slice(many1(attempt(char('a')) | char('b'))))(""))

  println(('a'.attempt | 'b').many.run("abba"))

  println(('a'.attempt | 'b').many.slice.run("abba"))
  println(('a'.attempt | 'b').many.slice.run("abcba"))

  println(('a'.attempt | 'b').times(3).slice.run("abb"))
  println(('a'.attempt | 'b').times(3).slice.run("abcb"))
}
