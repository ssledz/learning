package io.github.ssledz.http

sealed trait Method
case object POST extends Method
case object GET extends Method

case class Uri(path: String)

sealed trait Status
case object OK extends Status
case object NotFound extends Status

case class Request(
    method: Method,
    uri: Uri,
    body: String = ""
)
case class Response(
    status: Status,
    body: String = ""
)
