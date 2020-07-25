package io.github.ssledz.domain

import javax.persistence._

@MappedSuperclass
trait BaseEntity {

  protected var id: Long

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Access(AccessType.PROPERTY)
  def getId: Long = id

  def setId(id: Long): Unit = {
    this.id = id
  }

}
