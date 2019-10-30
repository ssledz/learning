package io.github.ssledz

import cats.effect.IO
import javax.servlet.AsyncContext

class AsyncHomeServlet extends ServletIO {

  def run(context: AsyncContext): IO[Unit] = {
    for {
      _ <- Schedulers.cpu.shift
      _ <- IO {
        context.getResponse.getWriter.print("Async Hello World !")
        println("Async Hello World !")
      }
    } yield ()


  }

}
