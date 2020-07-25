package io.github.ssledz

import com.typesafe.scalalogging.LazyLogging
import io.github.ssledz.domain.Employee
import javax.persistence.{EntityManagerFactory, Persistence}

object Main extends App with LazyLogging {

  val emf: EntityManagerFactory = Persistence.createEntityManagerFactory("default")

  val em = emf.createEntityManager()
  val employee = new Employee
  employee.setFirstName("Johny")
  employee.setLastName("Bravo")
  val tx = em.getTransaction
  tx.begin()
  em.persist(employee)
  tx.commit()
  logger.info("Saved {}", employee)

  emf.close()
}
