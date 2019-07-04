package io.github.ssledz.http.ver001

import io.github.ssledz.http._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object HttpImpl {

  type HttpApp = Request => Response

  val helloWorldApp: HttpApp = {
    case Request(POST, Uri("/hello"), name) => Response(OK, s"Hello, $name !")
    case _ => Response(NotFound)
  }

  val app: HttpApp = {
    case Request(POST, Uri("/translate"), text) =>
      val resp = Translator.future(text).map(Response(OK, _))
      Await.result(resp, 3.second)
    case _ => Response(NotFound)
  }

}
