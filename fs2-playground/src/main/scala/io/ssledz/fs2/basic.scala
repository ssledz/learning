package io.ssledz.fs2

import cats.effect.IO
import fs2.{INothing, Pure, Stream}

object basic extends App {

  val s0: Stream[Pure, INothing] = Stream.empty

  println(s0.toList)

  val s1: Stream[Pure, Int] = Stream(1,2,3)

  println(s1.toList)

  val s2: Stream[Pure, Int] = Stream.emit(1)

  println(s2.toList)

  val s3: Stream[Pure, Int] = Stream.emits(List(1, 2, 3))

  println(s3.toList)

  val s4 = Stream.fromIterator[IO](List(1,2,3).iterator)

}
