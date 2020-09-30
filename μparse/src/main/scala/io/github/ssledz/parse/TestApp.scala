package io.github.ssledz.parse
import Parsers._
object TestApp extends App {

  val abc = string("abc")

  println(run(abc)("abcdefg"))
  println(run(abc)("1abcdefg"))


}
