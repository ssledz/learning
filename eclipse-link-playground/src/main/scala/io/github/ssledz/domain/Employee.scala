package io.github.ssledz.domain

import java.util

import javax.persistence._

import scala.beans.BeanProperty
import scala.jdk.CollectionConverters._

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

  @ManyToMany
  @JoinTable(name = "proj_emp", joinColumns = Array(new JoinColumn(name = "emp_id")), inverseJoinColumns = Array(new JoinColumn(name = "proj_id")))
  private var projects: util.List[Project] = new util.ArrayList[Project]()

  def getProjects: List[Project] = projects.asScala.toList

  def addProject(p: Project): Project = {
    projects.add(p)
    p
  }

  override def toString: String = s"Employee(id=$id)"
}
