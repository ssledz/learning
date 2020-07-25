package io.github.ssledz.domain

import javax.persistence.{Access, AccessType, MappedSuperclass, Version}


@MappedSuperclass
trait VersionedEntity extends BaseEntity {

  protected var version: Long = 0L

  @Version
  @Access(AccessType.PROPERTY)
  def getVersion: Long = version

  def setVersion(version: Long): Unit = {
    this.version = version
  }

}
