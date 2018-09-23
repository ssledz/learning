package pl.softech.learning.ch3

import pl.softech.learning.ch3.List.Implicits._

import scala.annotation.tailrec

object Ex23 {

  def combine[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def combine(xs: List[A], ys: List[A], acc: List[A]): List[A] = (xs, ys) match {
      case (Cons(x, tl), Cons(y, tr)) => combine(tl, tr, Cons(f(x, y), acc))
      case (Nil, _) => acc
      case (_, Nil) => acc
    }

    combine(xs, ys, Nil).revers
  }

  trait Implicits {

    implicit class Ex23ListOpts[A](l: List[A]) {
      def combine(ys: List[A], f: (A, A) => A): List[A] = Ex23.combine(l, ys)(f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).combine(List(2, 2), _ + _))
  }

}
