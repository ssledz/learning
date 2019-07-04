package io.github.ssledz.http.ver002

import io.github.ssledz.http.ver002.HttpImpl._
import io.github.ssledz.http.{POST, Request, Uri}
import org.scalatest.AsyncFunSuite

class HttpImplTest extends AsyncFunSuite {

  test("testApp") {

    val req = Request(POST, Uri("/translate"), "one")

    app(req).map { resp =>
      assert("uno" === resp.body)
    }

  }

  test("combinedApp") {

    val req = Request(POST, Uri("/hello"), "Bob")

    combinedApp(req).map { resp =>
      assert("Hello, Bob !" === resp.body)
    }

    val req2 = Request(POST, Uri("/hola"), "Bob")

    combinedApp(req2).map { resp =>
      assert("Hola, Bob !" === resp.body)
    }

  }

}
