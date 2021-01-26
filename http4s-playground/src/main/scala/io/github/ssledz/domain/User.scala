package io.github.ssledz.domain

case class User(id: Long, userName: String, email: String)

case class CreateUserDto(userName: String, email: String)
