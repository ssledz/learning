package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex11 {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).toList)
  }

}
