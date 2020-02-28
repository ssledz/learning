package io.github.ssledz

import com.google.inject.servlet.GuiceServletContextListener
import com.google.inject.{AbstractModule, Guice, Injector, Scopes}
import net.codingwell.scalaguice.ScalaModule
import org.eclipse.jetty.util.thread.QueuedThreadPool

object Main extends App {

  val mainInjector = Guice.createInjector(new ApplicationModule)

  class AppServletContextListener extends GuiceServletContextListener {
    def getInjector: Injector = mainInjector.createChildInjector(new ServletRegistration(mainInjector))
  }

  val server = EmbeddedServer(
    port = 8090,
    new QueuedThreadPool(10, 1, 120),
    new AppServletContextListener,
    contextPath = "/")

  server.start.join

}

class ApplicationModule extends AbstractModule with ScalaModule {
  override def configure(): Unit = {
    bind[SyncHomeServlet].in(Scopes.SINGLETON)
  }
}

class ServletRegistration(override val i: Injector) extends ScalaServletModule {
  override def configureServlets(): Unit = {
    serve("/sync-home").using[SyncHomeServlet]
    serveAsync("/async-home").using[AsyncHomeServlet]
  }
}