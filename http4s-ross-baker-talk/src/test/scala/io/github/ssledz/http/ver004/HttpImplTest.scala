package io.github.ssledz.http.ver004

import io.github.ssledz.http.ver004.HttpImpl.app
import io.github.ssledz.http.{NotFound, POST, Request, Uri}
import org.scalatest.AsyncFunSuite

class HttpImplTest extends AsyncFunSuite {

  test("testApp") {
    val req = Request(POST, Uri("/hello"), "Bob")

    app(req).map { resp =>
      assert("Hello, Bob !" === resp.body)
    }

    val req2 = Request(POST, Uri("/hola"), "Bob")

    app(req2).map { resp =>
      assert("Hola, Bob !" === resp.body)
    }

    val req3 = Request(POST, Uri("/guten-tag"), "Bob")

    app(req3).map { resp =>
      assert(NotFound === resp.status)
    }

  }

}
