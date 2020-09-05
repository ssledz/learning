package io.github.ssledz

import cats.free.Free
import cats.free.Free.liftF
import cats.{Id, ~>}

sealed trait CaseBranchA[A, B]

case class Switch[A](value: A) extends CaseBranchA[A, Unit]

case class Case[A, B](condition: A => Boolean, body: () => B) extends CaseBranchA[A, Option[B]]

case class Default[A](body: () => A) extends CaseBranchA[Unit, A]

case class Return[A, B]() extends CaseBranchA[A, B]

object IfBranchApp extends App {

  type CaseBranch[A, B] = Free[CaseBranchA[A, *], B]

  def _switch[A](value: A): CaseBranch[A, Unit] = liftF[CaseBranchA[A, *], Unit](Switch(value))

  def _case[A, B](condition: A => Boolean, body: () => B): CaseBranch[A, Option[B]] = liftF[CaseBranchA[A, *], Option[B]](Case(condition, body))

  def _default[A](body: () => A): CaseBranch[Unit, A] = liftF[CaseBranchA[Unit, *], A](Default(body))

  def _return[A, B]: CaseBranch[A, B] = liftF[CaseBranchA[A, *], B](Return())

  def program: CaseBranch[Int, String] =
    for {
      _ <- _switch(2)
      _ <- _case[Int, String](_ % 2 == 0, () => "even")
      _ <- _case[Int, String](_ % 2 == 1, () => "odd")
      b <- _return[Int, String]
    } yield b


  def impureCompiler[A]: CaseBranchA[A, *] ~> Id = new (CaseBranchA[A, *] ~> Id) {

    var input: Option[A] = None
    var returnValue: Any = null

    def apply[B](fa: CaseBranchA[A, B]): Id[B] = fa match {
      case Switch(value) =>
        input = Some(value)
        ()
      case Case(condition, body) =>
        if (condition(input.get)) {
          returnValue = body()
          Some(returnValue)
        } else {
          None
        }
      case Default(body) =>
        ???
      case Return() =>
        returnValue.asInstanceOf[B]
    }
  }

  val result: String = program.foldMap(impureCompiler)

  println(result)

}