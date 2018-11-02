package pl.softech.learning.ch7

import java.util.concurrent.Executors

import pl.softech.learning.ch7.Par.Par

object Scratch extends App {

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  val es = Executors.newFixedThreadPool(5)
  val runner = Par.run[Int](es) _

  println(runner(sum(1 to 5)))

}
