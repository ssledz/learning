package io.github.ssledz.http.ver004

import io.github.ssledz.http._

import scala.concurrent.Future

object HttpImpl {

  type HttpApp = Request => Future[Response]

  type HttpRoutes = Request => Option[Future[Response]]

  object HttpRoutes {

    def of(pf: PartialFunction[Request, Future[Response]]): HttpRoutes = pf.lift

  }

  val helloWorld: HttpRoutes = HttpRoutes.of {
    case Request(POST, Uri("/hello"), name) => Future.successful(Response(OK, s"Hello, $name !"))
  }

  val holaMundo: HttpRoutes = HttpRoutes.of {
    case Request(POST, Uri("/hola"), name) => Future.successful(Response(OK, s"Hola, $name !"))
  }

  def combine(x: HttpRoutes, y: HttpRoutes): HttpRoutes = req => x(req).orElse(y(req))

  def seal(routes: HttpRoutes): HttpApp = routes.andThen(_.getOrElse(Future.successful(Response(NotFound))))

  val app: HttpApp = seal(combine(helloWorld, holaMundo))

}
