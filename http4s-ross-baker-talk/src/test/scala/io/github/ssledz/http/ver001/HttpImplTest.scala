package io.github.ssledz.http.ver001

import io.github.ssledz.http._
import io.github.ssledz.http.ver001.HttpImpl._
import org.scalatest.FunSuite

class HttpImplTest extends FunSuite {

  test("testHelloWorldApp") {

    val req = Request(POST, Uri("/hello"), "Boston")

    assert("Hello, Boston !" === helloWorldApp(req).body)

  }

  test("testApp") {

    val req = Request(POST, Uri("/translate"), "one")

    assert("uno" === app(req).body)

  }

}
