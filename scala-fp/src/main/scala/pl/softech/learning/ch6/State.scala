package pl.softech.learning.ch6

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (x, ss) = run(s)
    val newState = f(x)
    newState.run(ss)
  })

  def map[B](f: A => B): State[S, B] = flatMap(x => State(s => (f(x), s)))

  def map2[A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- sa
    b <- sb
  } yield f(a, b)

}

object State {

  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] = ???

}
