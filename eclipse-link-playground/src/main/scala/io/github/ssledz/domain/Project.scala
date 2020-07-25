package io.github.ssledz.domain

import javax.persistence._


@Entity
@Table(name = "project")
@Access(AccessType.PROPERTY)
case class Project(var name: String, protected var id: Long = -1) extends VersionedEntity {

  @Column(name = "project_name")
  def getName: String = name

  def setName(name: String): Unit = {
    this.name = name
  }

  private def this() {
    this(null)
  }
}
