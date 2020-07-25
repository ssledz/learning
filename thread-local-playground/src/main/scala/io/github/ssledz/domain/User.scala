package io.github.ssledz.domain

import io.github.ssledz.{JpaService, Transactor}

case class User(name: String)

class UserDao {
  def saveUser(u: User)(implicit tr: Transactor): Unit = tr.em.persist(u)
}

class JpaUserDao(jpa: JpaService) {
  def saveUser(u: User): Unit = jpa.em.persist(u)
}