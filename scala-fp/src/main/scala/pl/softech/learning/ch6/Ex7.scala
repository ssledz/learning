package pl.softech.learning.ch6

import pl.softech.learning.ch6.RNG._

object Ex7 {

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  trait Implicits {

    implicit class Ex7Opts(r: RNG) {
    }

  }

  def main(args: Array[String]): Unit = {

  }

}
