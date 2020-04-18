package io.github.ssledz

import io.github.ssledz.Tagger.{@@, tag}

sealed trait Maybe

object Maybe {

  implicit class MaybeOps[A](val a: A @@ Maybe) extends AnyVal {
    def getOrElse(value: => A): A = Maybe.getOrElse(a, value)
    def orElse(other: A @@ Maybe): A @@ Maybe = if (Maybe.isJust(a)) a else other
    def flatMap[B](f: A => B @@ Maybe): B @@ Maybe = Maybe.flatMap[A, B](a)(f)
    def map[B](f: A => B): B @@ Maybe = Maybe.map[A, B](a)(f)
  }

  private val tagger = tag[Maybe]

  def just[@specialized A](a: A) = tagger(a)

  def none[A] = tagger(null.asInstanceOf[A])

  def apply[A](a: A): A @@ Maybe = if (a != null) just(a) else none

  def isJust[A](a: A @@ Maybe): Boolean = a != null

  def isEmpty[A](a: A @@ Maybe): Boolean = a == null

  def getOrElse[A](a: A @@ Maybe, ifEmpty: => A): A = if (isJust(a)) a else ifEmpty

  def flatMap[A, B](a: A @@ Maybe)(f: A => B @@ Maybe): B @@ Maybe = if (Maybe.isJust(a)) f(a) else Maybe.none[B]

  def map[A, B](a: A @@ Maybe)(f: A => B): B @@ Maybe = flatMap(a)(a => Maybe.just(f(a)))

  def fold[A, B](a: A @@ Maybe)(ifEmpty: => B)(f: A => B): B = if (a == null) ifEmpty else f(a)
}
