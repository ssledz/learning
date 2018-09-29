package pl.softech.learning.ch3

object Ex29 {

  def fold[A, B](z: B)(t: Tree[A], f: (A, B) => B): B = ???

  def size[A](t: Tree[A]): Int = ???

  def depth[A](t: Tree[A]): Int = ???

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = ???

  def maximum(t: Tree[Int]): Int = ???

  trait Implicits {

    implicit class Ex29TreeOpts[A](t: Tree[A]) {
      def fold[B](z: B)(f: (A, B) => B): B = Ex29.fold(z)(t, f)
    }

  }

  def main(args: Array[String]): Unit = {
    println(map(Branch(Leaf(2), Branch(Leaf(1), Leaf(2))))(_ + 2))
    println(maximum(Branch(Leaf(2), Branch(Leaf(3), Leaf(2)))))
    println(depth(Branch(Leaf(2), Branch(Leaf(3), Leaf(2)))))
    println(size(Branch(Leaf(2), Branch(Leaf(3), Leaf(2)))))
  }

}
