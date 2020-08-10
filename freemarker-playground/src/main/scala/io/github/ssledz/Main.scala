package io.github.ssledz

import java.io.StringWriter
import java.util

import freemarker.cache.SoftCacheStorage
import freemarker.template.{Configuration, TemplateExceptionHandler}
import io.github.ssledz.Main.TestDataModel.ProductBean
import io.github.ssledz.TemplateService.{DataModel, TemplateIdentifier}

trait TemplateService {

  def render(identifier: TemplateIdentifier, data: DataModel): String

}

class DefaultTemplateService extends TemplateService {

  private val config: Configuration = {
    val cfg = new Configuration(Configuration.VERSION_2_3_29)
    cfg.setDefaultEncoding("UTF-8")
    cfg.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER)
    cfg.setLogTemplateExceptions(false)
    cfg.setWrapUncheckedExceptions(true)
    cfg.setFallbackOnNullLoopVariable(false)
    cfg.setCacheStorage(new SoftCacheStorage)
    cfg.setClassLoaderForTemplateLoading(classOf[DefaultTemplateService].getClassLoader, "templates")
    cfg
  }

  def render(identifier: TemplateIdentifier, data: DataModel): String = {
    val template = config.getTemplate(identifier.value)
    val out = new StringWriter()
    template.process(data.root, out)
    out.toString
  }
}

object TemplateService {

  trait DataModel {
    def root: Any
  }

  object DataModel {

    private case class DefaultDataModel(root: java.util.Map[String, Any]) extends DataModel

    def fromProduct(value: Product): DataModel = {

      val root = new util.HashMap[String, Any]()
      val names = value.productElementNames
      val elements = value.productIterator

      while (names.hasNext && elements.hasNext) {
        root.put(names.next(), elements.next())
      }

      DefaultDataModel(root)

    }
  }

  class TemplateIdentifier(val value: String) extends AnyVal

}

object Main extends App {


  case class TestDataModel(user: String, latestProduct: ProductBean)

  object TestDataModel {

    case class ProductBean(url: String, name: String)

  }

  val testTemplate = new TemplateIdentifier("test.ftlh")

  val service = new DefaultTemplateService
  val model = DataModel.fromProduct(TestDataModel("slavik", ProductBean("wp.pl", "wp")))
  val rendered = service.render(testTemplate, model)
  println(rendered)

}
