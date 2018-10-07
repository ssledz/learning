package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex7 {

  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = ???

  def flatMap[A, B](s: Stream[A])(f: A => Stream[B]): Stream[B] = ???

  def filter[A](s: Stream[A])(p: A => Boolean): Stream[A] = ???

  def append[A](s: Stream[A], a: A): Stream[A] = ???

  trait Implicits {

    implicit class Ex7Opts[A](s: Stream[A]) {

      def map[B](f: A => B): Stream[B] = Ex7.map(s)(f)

      def flatMap[B](f: A => Stream[B]): Stream[B] = Ex7.flatMap(s)(f)

      def filter(p: A => Boolean): Stream[A] = Ex7.filter(s)(p)

      def append(a: A): Stream[A] = Ex7.append(s, a)
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).map(_ + 111).toList)
    println(Stream(1, 2, 3, 4).append(5).append(6).toList)
    println(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList)
    println(Stream(1, 2, 3, 4).flatMap(e => Stream.cons(e, Stream.cons(e, Empty))).toList)
  }

}
