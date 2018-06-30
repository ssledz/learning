package pl.softech.typeclass.playground.cats

import cats._
import cats.implicits._

object MyFunctor {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  object FunctorInstances {

    implicit val treeInstance: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(value) => Leaf(f(value))
      }
    }

  }


}

object FunctorTest extends App {

  import MyFunctor._
  import MyFunctor.FunctorInstances._

  val tree : Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Leaf(5)))

  println(tree)

  val treeFunctor = Functor[Tree]

  println(treeFunctor.map(tree)(_ * 2))

  val tree2 = tree.map(_ * 5)
  println(tree2)


}
