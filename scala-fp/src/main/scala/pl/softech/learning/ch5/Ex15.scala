package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex15 {

  trait Implicits {

    implicit class Ex15Opts[A](s: Stream[A]) {
    }

  }

  def main(args: Array[String]): Unit = {
    println(Stream(1, 2, 3, 4).toList)
  }

}
