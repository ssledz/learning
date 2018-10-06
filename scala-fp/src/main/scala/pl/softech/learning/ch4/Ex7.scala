package pl.softech.learning.ch4

object Ex7 {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???

  def main(args: Array[String]): Unit = {

  }

}
