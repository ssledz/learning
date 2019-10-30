package io.github.ssledz

import com.google.inject.servlet.ServletModule.ServletKeyBindingBuilder
import javax.servlet.http.HttpServlet

trait ScalaServletModule {
  implicit class ServletKeyScalaSupport(builder: ServletKeyBindingBuilder) {
    def using[A <: HttpServlet](implicit m: Manifest[A]): Unit = {
      builder.`with`(m.runtimeClass.asSubclass(classOf[HttpServlet]))
    }
  }
}
