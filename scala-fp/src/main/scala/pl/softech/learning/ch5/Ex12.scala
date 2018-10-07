package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex12 {

  def fibs: Stream[Int] = ???

  def from(n: Int): Stream[Int] = ???

  def constant[A](a: A): Stream[A] = ???

  def ones: Stream[Int] = ???

  def main(args: Array[String]): Unit = {
    println(fibs.take(10).toList)
    println(from(3).take(5).toList)
    println(constant(2).take(10).toList)
    println(ones.take(10).toList)
  }

}
