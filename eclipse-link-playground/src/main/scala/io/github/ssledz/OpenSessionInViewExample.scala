package io.github.ssledz

import com.typesafe.scalalogging.LazyLogging
import io.github.ssledz.domain.{Employee, Project}
import javax.persistence.{EntityManagerFactory, Persistence}

object OpenSessionInViewExample extends App with LazyLogging {

  val emf: EntityManagerFactory = Persistence.createEntityManagerFactory("default")

  val em = emf.createEntityManager()
  val employee = new Employee
  employee.setFirstName("Johny")
  employee.setLastName("Bravo")
  val project = Project("eclipse-link-playground")
  employee.addProject(project)
  val tx = em.getTransaction
  tx.begin()
  em.persist(employee)
  em.persist(project)
  tx.commit()
  logger.info("Saved {}", employee)
  logger.info("Saved {}", project)
  em.clear()
  emf.getCache.evictAll()

  val query = em.createQuery("select e from Employee e where e.id = :id", classOf[Employee])
  query.setParameter("id", employee.getId)
  val newEmp = query.getSingleResult
  logger.info("employee: {}", newEmp)

  logger.info("em.close()")
  em.detach(newEmp)
  em.close()
  emf.getCache.evictAll()
  logger.info("emf.close()")
  emf.close()
  Thread.sleep(2000)

  // calls db even if emf is closed !!!
  logger.info("{} projects: {}", newEmp, newEmp.getProjects)

  //  emf.close()
}
