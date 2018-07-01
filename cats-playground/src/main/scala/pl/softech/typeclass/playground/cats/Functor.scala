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

object CodecPlay {

  trait Codec[A] {

    self =>

    def encode(value: A): String

    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {

      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }

  }

  object Codec {

    def apply[A](implicit c: Codec[A]): Codec[A] = c

    def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

    def decode[A: Codec](value: String): A = Codec[A].decode(value)

  }

  object CodecSyntax {

    implicit class CodecOps[A](value: A) {

      def encode(implicit c : Codec[A]): String = c.encode(value)

    }

  }

  object CodecInstances {

    implicit val stringCodec: Codec[String] = new Codec[String] {

      override def encode(value: String): String = value

      override def decode(value: String): String = value
    }

    implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

    implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)

  }

}

object FunctorTest extends App {

  import MyFunctor.FunctorInstances._
  import MyFunctor._

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Leaf(5)))

  println(tree)

  val treeFunctor = Functor[Tree]

  println(treeFunctor.map(tree)(_ * 2))

  val tree2 = tree.map(_ * 5)
  println(tree2)


  val inc = (x: Int) => x + 1
  val optInc = Functor[Option].lift(inc)


  println(inc(1))
  println(optInc(Option(1)))

  val f1 = (a: Int) => a + 1
  val f2 = (a: Int) => a * 2
  val f3 = (a: Int) => a + "!"
  val f4 = f1.map(f2).map(f3)
  val f5 = f1.andThen(f2).andThen(f3)

  println(f4(13))
  println(f5(13))

  def doSomeMatchF[F[_]](arg: F[Int])(implicit functor: Functor[F]): F[Int] = {
    arg.map(x => x + 1)
  }

  def doSomeMatchM[M[_]](m: M[Int])(implicit monad: Monad[M]): M[Int] = for {
    x <- m
    y <- m
  } yield x + y

  println(doSomeMatchF(Option(1)))
  println(doSomeMatchF(None: Option[Int]))
  println(doSomeMatchF(List(1, 2)))
  println(doSomeMatchM(List(1, 2)))


  import CodecPlay.CodecInstances._
  import CodecPlay._

  println(Codec[String].decode("str"))
  println(Codec[String].encode("str"))
  println(Codec[Int].decode("123"))
  println(Codec[Int].encode(123))

  import CodecPlay.CodecSyntax._
  println(111.encode)

}
