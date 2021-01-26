package io.github.ssledz.domain

import cats.effect.Bracket
import cats.implicits._
import doobie.Transactor
import doobie.implicits._
trait Users[F[_]] {

  def findAll: F[List[User]]

  def findById(id: Long): F[Option[User]]

  def createUser(userName: String, email: String): F[Unit]

}

class LifeUsers[F[_]: Bracket[?[_], Throwable]](val xa: Transactor[F]) extends Users[F] {

  def findAll: F[List[User]] = sql"""select id, user_name, email from app.users""".query[User].to[List].transact(xa)

  def findById(id: Long): F[Option[User]] = ???

  def createUser(userName: String, email: String): F[Unit] =
    sql"insert into app.users (user_name, email) values ($userName, $email)".update.run.transact(xa).void
}

object LifeUsers {
  def apply[F[_]: Bracket[?[_], Throwable]](xa: Transactor[F]): Users[F] = new LifeUsers[F](xa)
}
