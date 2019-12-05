package io.github.ssledz

import com.google.inject.servlet.{GuiceServletContextListener, ServletModule}
import com.google.inject.{Guice, Injector, Scopes}
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
  def getInjector: Injector = Guice.createInjector(new ApplicationModule)
}

class ApplicationModule extends ServletModule with AsyncServletModule with ScalaModule with ScalaServletModule {

  override def configureServlets(): Unit = {
    bind[SyncHomeServlet].in(Scopes.SINGLETON)
    serve("/sync-home").using[SyncHomeServlet]

    asyncServlet(binder, getServletContext)
      .serve("/async-home").using[AsyncHomeServlet].build()

  }

}