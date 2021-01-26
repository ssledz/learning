package io.github.ssledz.config

final case class AppConfig(db: DatabaseConfig, server: ServerConfig)

final case class ServerConfig(host: String, port: Int)

