package io.github.ssledz

import cats.effect.{Blocker, ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Resource, Sync, Timer}
import com.typesafe.scalalogging.LazyLogging
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import io.circe.config.parser
import io.circe.generic.auto._
import io.circe.syntax._
import io.github.ssledz.config.{AppConfig, DatabaseConfig}
import io.github.ssledz.domain.LifeUsers
import io.github.ssledz.infrastructure.endpoints.{AppInfoEndpoints, UserEndpoints}
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.{Router, Server}

object Main extends IOApp with LazyLogging {

  def createServer[F[_]: ContextShift: ConcurrentEffect: Timer]: Resource[F, Server[F]] =
    for {
      conf <- Resource.liftF(parser.decodePathF[F, AppConfig]("app"))
      serverEc <- ExecutionContexts.cachedThreadPool[F]
      connEc <- ExecutionContexts.fixedThreadPool[F](conf.db.connections.poolSize)
      txnEc <- ExecutionContexts.cachedThreadPool[F]
      xa: HikariTransactor[F] <- DatabaseConfig.dbTransactor(conf.db, connEc, Blocker.liftExecutionContext(txnEc))
      _ <- Resource.liftF(Sync[F].delay(logger.info("app config:\n{}", conf.asJson)))
      usersRepository = LifeUsers(xa)
      httpApp = Router(
        "/app" -> AppInfoEndpoints.endpoints(),
        "/users" -> UserEndpoints.endpoints(usersRepository)
      ).orNotFound
      _ <- Resource.liftF(DatabaseConfig.initializeDb(conf.db))
      server <- BlazeServerBuilder[F](serverEc)
        .bindHttp(conf.server.port, conf.server.host)
        .withHttpApp(httpApp)
        .resource
    } yield server

  def run(args: List[String]): IO[ExitCode] = createServer.use(_ => IO.never).as(ExitCode.Success)
}
