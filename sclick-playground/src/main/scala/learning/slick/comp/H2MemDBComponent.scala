package learning.slick.comp

import learning.slick.DbComponent
import slick.jdbc.MySQLProfile

trait H2MemDBComponent extends DbComponent {

  val driver = MySQLProfile

  import driver.api._

  val db: Database = Database.forConfig("h2mem")

}