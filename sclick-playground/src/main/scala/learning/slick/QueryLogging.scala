package learning.slick

import org.slf4j.LoggerFactory
import slick.sql.SqlAction

trait QueryLogging {

  protected val logger = LoggerFactory.getLogger(this.getClass)

  protected def traceQa[A <: SqlAction[_, _, _]](qa: A) = {
    if (logger.isTraceEnabled) {
      logger.trace("query: " + qa.statements.mkString(""))
    }
    qa
  }

}
