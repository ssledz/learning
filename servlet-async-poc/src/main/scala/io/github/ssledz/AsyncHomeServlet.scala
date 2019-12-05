package io.github.ssledz

import cats.effect.IO
import javax.inject.{Inject, Singleton}
import javax.servlet.AsyncContext

@Singleton
class AsyncHomeServlet @Inject()(helloWorldService: HelloWorldService) extends ServletIO {

  def run(context: AsyncContext): IO[Unit] = {
    for {
      _ <- Schedulers.cpu.shift
      _ <- IO {
        context.getResponse.getWriter.print("Async " + helloWorldService.helloWorld)
        println("Async " + helloWorldService.helloWorld)
      }
    } yield ()


  }

}
