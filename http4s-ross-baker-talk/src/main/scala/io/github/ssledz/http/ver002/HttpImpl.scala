package io.github.ssledz.http.ver002

import io.github.ssledz.http._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object HttpImpl {

  type HttpApp = Request => Future[Response]

  val app: HttpApp = {
    case Request(POST, Uri("/translate"), text) =>
      Translator.future(text).map(Response(OK, _))
    case _ => Future.successful(Response(NotFound))
  }

  val helloWorld: HttpApp = {
    case Request(POST, Uri("/hello"), name) => Future.successful(Response(OK, s"Hello, $name !"))
  }

  val holaMundo: HttpApp = {
    case Request(POST, Uri("/hola"), name) => Future.successful(Response(OK, s"Hola, $name !"))
  }

  def combine(x: HttpApp, y: HttpApp): HttpApp = { req =>
    try {
      x(req)
    } catch {
      case _: MatchError => y(req)
    }
  }

  val combinedApp: HttpApp = combine(helloWorld, holaMundo)

}
