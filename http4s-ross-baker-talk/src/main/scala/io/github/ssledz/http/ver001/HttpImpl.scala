package io.github.ssledz.http.ver001

import io.github.ssledz.http._

object HttpImpl {

  type HttpApp = Request => Response

  val helloWorldApp: HttpApp = {
    case Request(POST, Uri("/hello"), name) => Response(OK, s"Hello, $name !")
    case _ => Response(NotFound)
  }

}
