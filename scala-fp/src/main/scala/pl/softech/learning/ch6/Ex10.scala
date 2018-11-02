package pl.softech.learning.ch6

import pl.softech.learning.ch6.Ex9.map
import pl.softech.learning.ch6.RNG.int

object Ex10 {

  type Rand[A] = State[RNG, A]

  val rndInt = new Rand[Int](_.nextInt)

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)
    println(map(int)(_ + 1)(rng))
    println(rndInt.map(_ + 1).run(rng))

  }

}
