package io.github.ssledz.http.ver003

import io.github.ssledz.http.ver003.HttpImpl.app
import io.github.ssledz.http.{POST, Request, Uri}
import org.scalatest.AsyncFunSuite

import scala.concurrent.Future

class HttpImplTest extends AsyncFunSuite {

  test("app") {

    val req = Request(POST, Uri("/hello"), "Bob")

    app(req).map { resp =>
      assert("Hello, Bob !" === resp.body)
    }

    val req2 = Request(POST, Uri("/hola"), "Bob")

    app(req2).map { resp =>
      assert("Hola, Bob !" === resp.body)
    }

    val req3 = Request(POST, Uri("/guten-tag"), "Bob")

    recoverToSucceededIf[MatchError] {
      Future(app(req3))
    }

  }

}
