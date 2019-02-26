package learning.slick.mysql

import learning.slick.DbComponent
import slick.jdbc.MySQLProfile

trait MySqlDBComponent extends DbComponent {

  val driver = MySQLProfile

  import driver.api._

  val db: Database = Database.forConfig("mysql")

}