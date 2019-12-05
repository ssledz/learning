package io.github.ssledz

import javax.inject.{Inject, Singleton}
import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

@Singleton
class SyncHomeServlet @Inject()(helloWorldService: HelloWorldService) extends HttpServlet {

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse): Unit = handle(req, resp)

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse): Unit = handle(req, resp)

  def handle(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    resp.getWriter.print("Sync " + helloWorldService.helloWorld)
    println("Sync " + helloWorldService.helloWorld)
  }

}
