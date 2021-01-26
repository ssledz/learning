package io.github.ssledz.infrastructure.endpoints

import buildinfo.BuildInfo
import cats.effect.Sync
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.http4s.circe.jsonEncoderOf
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityEncoder, HttpRoutes}

class AppInfoEndpoints[F[_]: Sync] extends Http4sDsl[F] {

  private implicit val buildInfoJsonEncoder: Encoder[BuildInfo.type] = (a: BuildInfo.type) =>
    Json.obj(
      ("name", a.name.asJson),
      ("version", a.version.asJson),
      ("scalaVersion", a.scalaVersion.asJson),
      ("sbtVersion", a.sbtVersion.asJson)
  )

  private implicit val buildInfoEntityEncoder: EntityEncoder[F, BuildInfo.type] = jsonEncoderOf[F, BuildInfo.type]

  private def buildInfoEndpoint(): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "build-info" => Ok(BuildInfo)
    }

  def endpoints(): HttpRoutes[F] = buildInfoEndpoint()
}

object AppInfoEndpoints {
  def endpoints[F[_]: Sync](): HttpRoutes[F] = new AppInfoEndpoints[F].endpoints()
}
