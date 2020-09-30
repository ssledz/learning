package io.github.ssledz.parse

import scala.util.matching.Regex

object Parsers {

  type Parser[+A] = Location => Result[A]

  def run[A](p: Parser[A])(input: String): Result[A] = p(Location(input, 0))

  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

  def succeed[A](a: A): Parser[A] = map(string(""))(_ => a)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(f andThen succeed)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(p, p2))(f.tupled)

  def many[A](p: Parser[A]): Parser[List[A]] = or(map2(p, many(p))(_ :: _), succeed(List.empty))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List.empty[A]) else map2(p, listOfN(n - 1, p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def string(s: String): Parser[String] =
    scope(s"Expected: $s") { loc =>
      if (loc.input.startsWith(s)) Success(s, s.length) else Failure.Empty
    }

  def regex(r: Regex): Parser[String] = scope(s"Expected: $r") { loc =>
    r.findFirstIn(loc.input) match {
      case Some(value) => Success(value, value.length)
      case None => Failure.Empty
    }
  }


  def scope[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.push(loc, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.label(msg))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  def many1[A](p: Parser[A]): Parser[List[A]] = ???

  def slice[A](p: Parser[A]): Parser[String] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = loc => p(loc) match {
    case Success(a, charsConsumed) => f(a)(loc.advanceBy(charsConsumed))
    case err@Failure(_) => err
  }

  implicit def ops[A](p: Parser[A]): ParserOps[A] = new ParserOps[A](p)

  class ParserOps[A](val p: Parser[A]) extends AnyVal {

    def flatMap[B](f: A => Parser[B]): Parser[B] = Parsers.flatMap(p)(f)

    def map[B](f: A => B): Parser[B] = Parsers.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)] = Parsers.product(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = Parsers.or(p, p2)

    def |[B >: A](p2: => Parser[B]): Parser[B] = Parsers.or(p, p2)

  }

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

    def latest: Option[(Location, String)] = stack.lastOption

    def latestLoc: Option[Location] = latest map (_._1)

    def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
  }

  case class Location(input: String, offset: Int) {
    lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def advanceBy(n: Int): Location = copy(offset = offset + n)

    def toError(msg: String): ParseError = ParseError(List(this -> msg))
  }

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]

  object Failure {
    val Empty: Failure = Failure(ParseError(List.empty))
  }

}
