package io.github.ssledz.domain

import javax.persistence._

import scala.beans.BeanProperty

@Entity
@Table(name = "employee")
class Employee {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private var id: Long = _

  @Version private var version: Long = 0L

  @BeanProperty
  @Column(name = "first_name")
  var firstName: String = _

  @BeanProperty
  @Column(name = "last_name")
  var lastName: String = _

  def getId: Long = id

  override def toString: String = s"Employee(id=$id)"
}
