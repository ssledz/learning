package io.github.ssledz.config

import cats.effect.{Async, Blocker, ContextShift, Resource, Sync}
import cats.implicits._
import doobie.hikari.HikariTransactor
import org.flywaydb.core.Flyway

import scala.concurrent.ExecutionContext

final case class DatabaseConfig(
    url: String,
    driver: String,
    user: String,
    password: String,
    connections: DatabaseConnectionsConfig
)

final case class DatabaseConnectionsConfig(poolSize: Int)

object DatabaseConfig {

  def dbTransactor[F[_]: Async: ContextShift](
      dbc: DatabaseConfig,
      connEc: ExecutionContext,
      blocker: Blocker,
  ): Resource[F, HikariTransactor[F]] =
    HikariTransactor.newHikariTransactor[F](dbc.driver, dbc.url, dbc.user, dbc.password, connEc, blocker)

  def initializeDb[F[_]](cfg: DatabaseConfig)(implicit S: Sync[F]): F[Unit] =
    S.delay {
        val fw = Flyway
          .configure()
          .dataSource(cfg.url, cfg.user, cfg.password)
          .load()
        fw.migrate()
      }
      .as(())
}
