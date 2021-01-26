package io.github.ssledz.infrastructure.endpoints

import cats.effect.Sync
import cats.implicits._
import io.github.ssledz.domain.{CreateUserDto, Users}
import io.github.ssledz.infrastructure.endpoints.json._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

class UserEndpoints[F[_]: Sync](users: Users[F]) extends Http4sDsl[F] {

  private def buildInfoEndpoint(): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root => Ok(users.findAll)
      case request @ POST -> Root =>
        (for {
          dto <- request.as[CreateUserDto]
          _ <- users.createUser(dto.userName, dto.email)
        } yield ()) *> Created()
    }

  def endpoints(): HttpRoutes[F] = buildInfoEndpoint()
}

object UserEndpoints {
  def endpoints[F[_]: Sync](users: Users[F]): HttpRoutes[F] = new UserEndpoints[F](users).endpoints()
}
