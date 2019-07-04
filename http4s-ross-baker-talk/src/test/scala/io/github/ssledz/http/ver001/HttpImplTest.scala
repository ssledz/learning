package io.github.ssledz.http.ver001

import io.github.ssledz.http._
import io.github.ssledz.http.ver001.HttpImpl.helloWorldApp
import org.scalatest.{Assertions, FunSuite}

class HttpImplTest extends FunSuite with Assertions {

  test("testHelloWorldApp") {

    val req = Request(POST, Uri("/hello"), "Boston")

    assert("Hello, Boston !" === helloWorldApp(req).body)

  }

}
