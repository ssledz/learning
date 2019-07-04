package io.github.ssledz.http.ver003

import io.github.ssledz.http._

import scala.concurrent.Future

object HttpImpl {

  type HttpApp = Request => Future[Response]

  type HttpRoutes = PartialFunction[Request, Future[Response]]

  val helloWorld: HttpRoutes = {
    case Request(POST, Uri("/hello"), name) => Future.successful(Response(OK, s"Hello, $name !"))
  }

  val holaMundo: HttpRoutes = {
    case Request(POST, Uri("/hola"), name) => Future.successful(Response(OK, s"Hola, $name !"))
  }

  def combine(x: HttpRoutes, y: HttpRoutes): HttpApp = x orElse y

  val app: HttpApp = combine(helloWorld, holaMundo)

}
