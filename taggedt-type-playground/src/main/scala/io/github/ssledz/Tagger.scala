package io.github.ssledz

import io.github.ssledz.Tagger._

class Tagger[U] {
  def apply[T](t: T): T @@ U = t.asInstanceOf[T @@ U]
}

object Tagger {

  def tag[U] = new Tagger[U]

  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T with Tagged[U]
}
