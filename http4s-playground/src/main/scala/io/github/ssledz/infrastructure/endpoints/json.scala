package io.github.ssledz.infrastructure.endpoints

import cats.Applicative
import cats.effect.Sync
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import io.github.ssledz.domain.{CreateUserDto, User}
import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.circe.{jsonEncoderOf, jsonOf}

object json extends JsonCodecs {
  implicit def deriveEntityEncoder[F[_]: Applicative, A: Encoder]: EntityEncoder[F, A] = jsonEncoderOf[F, A]
  implicit def deriveEntityDecoder[F[_]: Sync, A: Decoder]: EntityDecoder[F, A] = jsonOf[F, A]
}

private[endpoints] trait JsonCodecs {

  implicit lazy val userDecoder: Decoder[User] = deriveDecoder[User]
  implicit lazy val userEncoder: Encoder[User] = deriveEncoder[User]

  implicit lazy val userDtoDecoder: Decoder[CreateUserDto] = deriveDecoder[CreateUserDto]
  implicit lazy val userDtoEncoder: Encoder[CreateUserDto] = deriveEncoder[CreateUserDto]
}
