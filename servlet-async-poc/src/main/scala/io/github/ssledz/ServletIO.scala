package io.github.ssledz

import cats.effect.IO
import cats.implicits._
import javax.servlet.AsyncContext
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

trait ServletIO extends HttpServlet with AsyncServlet {

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse): Unit = handle(req)

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse): Unit = handle(req)

  private def handle(req: HttpServletRequest): Unit = {

    val ctx = IO.pure(req.startAsync())

    val task = Schedulers.cpu.shift *> ctx.bracket(run)(c => IO.pure(c).map(_.complete))

    task.unsafeRunAsyncAndForget

  }

  def run(context: AsyncContext): IO[Unit]


}
