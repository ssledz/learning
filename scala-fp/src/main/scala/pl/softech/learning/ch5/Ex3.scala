package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex3 {

  def takeWhile[A](s: Stream[A], p: A => Boolean): Stream[A] = ???

  trait Implicits {

    implicit class Ex3Opts[A](s: Stream[A]) {
      def takeWhile(p: A => Boolean): Stream[A] = Ex3.takeWhile(s, p)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).takeWhile(_ > 3).toList)
  }

}
