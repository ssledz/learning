package pl.softech.learning.ch7

import java.util.concurrent.Executors

import pl.softech.learning.ch7.Par._

object Ex4 extends App {

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(())) {
    (xs, _) => xs.sorted
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(())) {
    (a, _) => f(a)
  }

  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](xs: List[Par[A]]): Par[List[A]] = es => xs match {
    case h :: t => map2(h, sequence(t))(_ :: _)(es)
    case _ => unit(List.empty[A])(es)
  }

  val res = parMap(List(1, 2, 3, 4)) {
    x => {
      Thread.sleep(x * 1000)
      x * 2
    }
  }

  def run(res : Par[List[Int]])(nThreads : Int): Unit = {
    val start = System.currentTimeMillis()
    println("Start with " + nThreads + " threads")
    println(Par.run(Executors.newFixedThreadPool(nThreads))(res))
    println("end after: " + (System.currentTimeMillis() - start) + " ms")
  }

  val example = run(res) _

  example(4)
  example(1)

}
