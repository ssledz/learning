package io.github.ssledz.http

import scala.concurrent.Future

object Translator {

  def future(text: String): Future[String] = text match {
    case "one" => Future.successful("uno")
    case _ => Future.failed(new RuntimeException(s"can't translate $text"))
  }

}
