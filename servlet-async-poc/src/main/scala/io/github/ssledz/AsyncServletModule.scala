package io.github.ssledz

import java.lang.ref.WeakReference

import cats.data._
import cats.implicits._
import com.google.inject.Binder
import io.github.ssledz.AsyncServletModule.{AsyncServletBind, AsyncServletInitializationService, ServletContextHolder}
import javax.inject.{Inject, Singleton}
import javax.servlet.ServletContext
import net.codingwell.scalaguice.{InternalModule, ScalaMultibinder}

trait AsyncServletModule extends InternalModule[Binder] {

  def asyncServlet(binder: Binder, servletContext: ServletContext): MultiBindingAsyncBuilder = new MultiBindingAsyncBuilder(binder, servletContext, None, List.empty)

  class MultiBindingAsyncBuilder(binder: Binder, servletContext: ServletContext, urlPattern: Option[String], xs: List[State[ScalaMultibinder[AsyncServlet], AsyncServletBind]]) {
    def using[A <: AsyncServlet](implicit m: Manifest[A]): MultiBindingAsyncBuilder = urlPattern match {
      case Some(url) => {
        val s = State[ScalaMultibinder[AsyncServlet], AsyncServletBind] { mBinder =>
          mBinder.addBinding.to[A]
          (mBinder, AsyncServletBind[A](url))
        }
        new MultiBindingAsyncBuilder(binder, servletContext, None, s :: xs)
      }
      case None => throw new IllegalArgumentException("You can't call 'using' two times in a row")
    }

    def serve(urlPattern: String): MultiBindingAsyncBuilder = new MultiBindingAsyncBuilder(binder, servletContext, Some(urlPattern), xs)

    def build(): Unit = {
      val mBinder: ScalaMultibinder[AsyncServlet] = ScalaMultibinder.newSetBinder[AsyncServlet](binder)
      val binds = xs.sequence.runA(mBinder).value
      bind[Seq[AsyncServletBind]].toInstance(binds)
      bind[AsyncServletInitializationService].asEagerSingleton()
      bind[ServletContextHolder].toInstance(new ServletContextHolder(new WeakReference(servletContext)))
    }

  }

}

object AsyncServletModule {

  @Singleton
  private class AsyncServletInitializationService @Inject()(beans: Set[AsyncServlet], binds: Seq[AsyncServletBind])(implicit holder: ServletContextHolder) {

    val s2url = Map(binds.map(b => (b.className, b.urlPattern)): _*)

    beans.foreach(servlet => registerAsync(servlet, s2url(servlet.getClass.getName)))

    def registerAsync(servlet: AsyncServlet, urlPattern: String)(implicit ctx: ServletContextHolder): Unit = {
      val registration = ctx.servletContext.addServlet(servlet.getClass.getSimpleName, servlet)
      registration.setAsyncSupported(true)
      registration.addMapping(urlPattern)
    }

  }

  case class AsyncServletBind(className: String, urlPattern: String)

  object AsyncServletBind {
    def apply[T](urlPattern: String)(implicit m: Manifest[T]): AsyncServletBind = new AsyncServletBind(m.runtimeClass.getName, urlPattern)
  }

  private class ServletContextHolder(sc: WeakReference[ServletContext]) {
    def servletContext: ServletContext = sc.get()
  }

}