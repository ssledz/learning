package io.github.ssledz

import com.google.inject.Injector
import com.google.inject.servlet.ServletModule
import com.google.inject.servlet.ServletModule.ServletKeyBindingBuilder
import javax.servlet.http.HttpServlet
import net.codingwell.scalaguice.InjectorExtensions._

abstract class ScalaServletModule extends ServletModule {

  val i: Injector

  implicit class ServletKeyScalaSupport(builder: ServletKeyBindingBuilder) {
    def using[A <: HttpServlet](implicit m: Manifest[A]): Unit =
      builder.`with`(i.instance[A])
  }

  def serveAsync(path: String): AsyncServletKey =
    AsyncServletKey(path)

  case class AsyncServletKey(path: String) {
    def using[A <: AsyncServlet](implicit m: Manifest[A]): Unit = {
      val r = getServletContext.addServlet(m.runtimeClass.getSimpleName, i.instance[A])
      r.setAsyncSupported(true)
      r.addMapping(path)
    }
  }

}
