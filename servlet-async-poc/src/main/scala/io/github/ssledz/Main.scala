package io.github.ssledz

import com.google.inject.servlet.{GuiceServletContextListener, ServletModule}
import com.google.inject.{Guice, Injector, Scopes}
import javax.servlet.http.HttpServlet
import javax.servlet.{ServletContext, ServletContextEvent}
import net.codingwell.scalaguice.ScalaModule
import org.eclipse.jetty.util.thread.QueuedThreadPool

object Main extends App {

  val server = EmbeddedServer(
    port = 8090,
    new QueuedThreadPool(10, 1, 120),
    new AppServletContextListener,
    contextPath = "/")

  server.start.join

}

class AppServletContextListener extends GuiceServletContextListener {

  private def registerAsync[S <: HttpServlet](urlPattern: String)(implicit m: Manifest[S], ctx: ServletContext): Unit = {
    val clazz = m.runtimeClass.asSubclass(classOf[HttpServlet])
    val registration = ctx.addServlet(clazz.getSimpleName, clazz)
    registration.setAsyncSupported(true)
    registration.addMapping(urlPattern)

  }

  override def contextInitialized(ev: ServletContextEvent): Unit = {
    super.contextInitialized(ev)
    implicit val ctx = ev.getServletContext
    registerAsync[AsyncHomeServlet]("/async-home")
  }

  override def getInjector: Injector = Guice.createInjector(new ApplicationModule)
}

class ApplicationModule extends ServletModule with ScalaModule with ScalaServletModule {

  override def configureServlets(): Unit = {
    bind[SyncHomeServlet].in(Scopes.SINGLETON)
    serve("/sync-home").using[SyncHomeServlet]
  }

}