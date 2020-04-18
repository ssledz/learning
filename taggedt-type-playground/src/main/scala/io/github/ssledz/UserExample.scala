package io.github.ssledz
import io.github.ssledz.Tagger._

object UserExample extends App {

  sealed trait UserIdTag
  sealed trait UserNameTag
  sealed trait PasswordTag

  type UserId = String @@ UserIdTag
  type UserName = String @@ UserNameTag
  type Password = String @@ PasswordTag

  case class User(id: UserId, username: UserName, password: Password)

  val user1 = User(tag[UserIdTag]("1"), tag[UserNameTag]("slack"), tag[PasswordTag]("criptic"))

  println(user1)
}
