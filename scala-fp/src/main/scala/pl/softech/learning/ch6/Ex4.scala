package pl.softech.learning.ch6

import pl.softech.learning.ch6.RNG.Implicits._

object Ex4 {

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  trait Implicits {

    implicit class Ex4Opts(r: RNG) {
      def ints(count: Int): (List[Int], RNG) = Ex4.ints(count)(r)
    }

  }

  def main(args: Array[String]): Unit = {
    println(SimpleRNG(1).ints(5))
  }

}
