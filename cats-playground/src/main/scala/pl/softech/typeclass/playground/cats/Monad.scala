package pl.softech.typeclass.playground.cats

object MyMonad {

  trait Monad[M[_]] {

    def pure[A](a: A): M[A]

    def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]

    def map[A, B](a: M[A])(f: A => B): M[B] = flatMap(a)(x => pure(f(x)))

  }

  object Monad {
    def apply[M[_]](implicit m: Monad[M]): Monad[M] = m
  }

  trait Box[+A]

  case class SimpleBox[A](value: A) extends Box[A]

  case object EmptyBox extends Box[Nothing]

  object Box {
    def apply[A](value: A): Box[A] = SimpleBox(value)
  }

  object MonadInstances {

    implicit val boxInstance: Monad[Box] = new Monad[Box] {

      override def pure[A](a: A): Box[A] = SimpleBox(a)

      override def flatMap[A, B](a: Box[A])(f: A => Box[B]): Box[B] = a match {
        case EmptyBox => EmptyBox
        case SimpleBox(value) => f(value)
      }
    }

  }

  object MonadSyntax {

    implicit class MonadOps[M[_], A](value: M[A]) {

      def flatMap[B](f: A => M[B])(implicit m: Monad[M]): M[B] = m.flatMap(value)(f)

      def map[B](f: A => B)(implicit m: Monad[M]): M[B] = m.map(value)(f)

    }

  }


}

object MyMonadTest extends App {

  import MyMonad.MonadInstances._
  import MyMonad.MonadSyntax._
  import MyMonad._

  val m: Monad[Box] = Monad[Box]
  println(m.flatMap(m.pure(1))(a => m.pure(a + 1)))
  println(m.flatMap(EmptyBox: Box[Int])(a => m.pure(a + 1)))

  println(Box(1).flatMap(x => SimpleBox(x + 1)))
  println(Box(1).flatMap(x => EmptyBox))
  println(Box(1).map(x => x + 1))

}
