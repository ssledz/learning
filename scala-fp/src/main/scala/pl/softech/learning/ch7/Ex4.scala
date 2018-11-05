package pl.softech.learning.ch7

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

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs : List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](xs : List[Par[A]]) : Par[List[A]] = es => xs match {
    case h :: t => map2(h, sequence(t))(_ :: _)(es)
    case _ => unit(List.empty[A])(es)
  }

}
