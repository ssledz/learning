package pl.softech.learning.ch5

object Ex5 {

  def takeWhile[A](s: Stream[A], p: A => Boolean): Stream[A] = ???


  def main(args: Array[String]): Unit = {
    println(takeWhile[Int](Stream(1, 2, 3, 4), _ > 3))
  }

}
