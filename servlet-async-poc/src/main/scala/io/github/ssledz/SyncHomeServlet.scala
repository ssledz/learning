package io.github.ssledz

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}

class SyncHomeServlet extends HttpServlet {

  override def doGet(req: HttpServletRequest, resp: HttpServletResponse): Unit = handle(req, resp)

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse): Unit = handle(req, resp)

  def handle(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    resp.getWriter.print("Sync Hello World !")
    println("Sync Hello World !")
  }

}
