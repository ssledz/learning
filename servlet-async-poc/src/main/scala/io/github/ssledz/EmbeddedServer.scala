package io.github.ssledz

import java.lang.management.ManagementFactory
import java.rmi.registry.LocateRegistry
import java.util
import java.util.EventListener

import com.google.inject.servlet.GuiceFilter
import javax.management.remote.{JMXConnectorServerFactory, JMXServiceURL}
import javax.servlet.DispatcherType
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHandler}
import org.eclipse.jetty.util.thread.QueuedThreadPool

import scala.collection.JavaConverters._


case class EmbeddedServer(port: Int,
                          threadPool: QueuedThreadPool,
                          listener: EventListener,
                          contextPath: String,
                          registryPort: Option[Int] = None) {

  private val server = createServer(createContextHandler(createServletHandler()))

  def start: Server = {
    registryPort.foreach(createJmxConnectorServer)
    server.start()
    server
  }

  private def getResource(name: String): String = {
    classOf[EmbeddedServer].getClassLoader.getResource(name).getFile
  }

  private def createJmxConnectorServer(registryPort: Int): Unit = {

    val env: Map[String, Object] = Map(
      "jmx.remote.x.password.file" -> getResource("jmx-remote.password"),
      "jmx.remote.x.access.file" -> getResource("jmx-remote.access")
    )

    LocateRegistry.createRegistry(registryPort)
    val mbs = ManagementFactory.getPlatformMBeanServer
    val url = new JMXServiceURL(s"service:jmx:rmi://localhost/jndi/rmi://localhost:$registryPort/jmxrmi")
    val svr = JMXConnectorServerFactory.newJMXConnectorServer(url, env.asJava, mbs)
    svr.start()
  }

  private def createServer(context: ServletContextHandler): Server = {
    val server = new Server(threadPool)
    val connector = createConnector(server)
    server.setConnectors(Array(connector))
    server.setHandler(context)
    server
  }

  private def createConnector(server: Server): ServerConnector = {
    val connector = new ServerConnector(server)
    connector.setPort(port)
    connector
  }

  private def createContextHandler(handler: ServletHandler): ServletContextHandler = {
    val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
    context.setContextPath(contextPath)
    context.setClassLoader(classOf[EmbeddedServer].getClassLoader)
    context.addEventListener(listener)
    context.setServletHandler(handler)
    context
  }

  private def createServletHandler(): ServletHandler = {
    val handler = new ServletHandler
    handler.addFilterWithMapping(classOf[GuiceFilter], "/*", util.EnumSet.allOf(classOf[DispatcherType]))
    handler
  }
}
