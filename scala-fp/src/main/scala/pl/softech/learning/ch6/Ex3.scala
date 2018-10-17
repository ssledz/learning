package pl.softech.learning.ch6

import pl.softech.learning.ch6.RNG.Implicits._

object Ex3 {

  def intDouble(rng: RNG): ((Int, Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  trait Implicits {

    implicit class Ex3Opts(r: RNG) {

      def intDouble: ((Int, Double), RNG) = Ex3.intDouble(r)

      def doubleInt: ((Double, Int), RNG) = Ex3.doubleInt(r)

      def double3: ((Double, Double, Double), RNG) = Ex3.double3(r)

    }

  }

  def main(args: Array[String]): Unit = {
    println(SimpleRNG(1).intDouble)
    println(SimpleRNG(2).doubleInt)
    println(SimpleRNG(3).double3)
  }

}
