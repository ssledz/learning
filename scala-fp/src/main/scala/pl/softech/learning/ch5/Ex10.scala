package pl.softech.learning.ch5

import pl.softech.learning.ch5.Stream.Implicits._

object Ex10 {

  def fibs: Stream[Int] = ???

  def main(args: Array[String]): Unit = {
    println(fibs.take(10).toList)
  }

}
