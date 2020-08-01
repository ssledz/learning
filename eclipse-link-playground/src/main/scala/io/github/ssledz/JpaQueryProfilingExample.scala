package io.github.ssledz

import io.github.ssledz.domain.{Employee, Project}
import javax.persistence.{EntityManagerFactory, Persistence, PersistenceUnitUtil}
import org.eclipse.persistence.annotations.BatchFetchType
import org.eclipse.persistence.config.QueryHints
import org.eclipse.persistence.internal.jpa.EntityManagerFactoryDelegate
import org.eclipse.persistence.queries.LoadGroup
import org.eclipse.persistence.sessions.SessionProfiler
import org.eclipse.persistence.tools.profiler.PerformanceMonitor

import scala.jdk.CollectionConverters._

/**
 * This example requires that all entities are properly waved by eclipseLink
 *
 * In order to make eclipseLink happy download java agent
 * wget -O /tmp/eclipselink.jar https://repo1.maven.org/maven2/org/eclipse/persistence/eclipselink/2.7.7/eclipselink-2.7.7.jar
 * pass following parameter -javaagent:/tmp/eclipselink.jar to jvm
 * and set 'eclipselink.weaving' to true in persistance.xml
 *
 */
object JpaQueryProfilingExample extends App {

  val emf: EntityManagerFactory = Persistence.createEntityManagerFactory("default")
  val pUtil: PersistenceUnitUtil = emf.getPersistenceUnitUtil
  val emfd: EntityManagerFactoryDelegate = emf.unwrap(classOf[EntityManagerFactoryDelegate])
  val profiler: PerformanceMonitor = emfd.getServerSession.getProfiler.asInstanceOf[PerformanceMonitor]

  val em = emf.createEntityManager()

  clearDb()

  val johny = createEmployee("Johny", "Bravo", "eclipse-link-playground")
  val tom = createEmployee("Tom", "Tip", "eclipse-link-profiling")

  em.clear()
  emf.getCache.evictAll()

  val stats = QueryStatistics(profiler)

  val employees = findAll()

  def numberOfDbQueries: Int = QueryStatistics(profiler).diff(stats).numberOfDbQueries

  assert(employees.size == 2)
  assert(numberOfDbQueries == 1)

  assertNotLoaded(employees.head, "projects")

  employees.foreach(_.getProjects) // trigger lazy loading

  assert(numberOfDbQueries == 3, "+2 for lazy loaded project")

  private def clearDb(): Unit = {
    val tx = em.getTransaction
    tx.begin()
    em.createQuery("delete from Employee").executeUpdate()
    em.createQuery("delete from Project").executeUpdate()
    tx.commit()
  }

  private def findAll(): List[Employee] =
    em.createQuery("select e from Employee e", classOf[Employee]).getResultList.asScala.toList

  private def findAllWithProjects(): List[Employee] = {
    val query = em.createQuery("select e from Employee e", classOf[Employee])
    query.setHint(QueryHints.LOAD_GROUP, loadGroup("projects"))
    query.setHint(QueryHints.BATCH_TYPE, BatchFetchType.EXISTS)
    query.setHint(QueryHints.BATCH, "e.projects")
    query.getResultList.asScala.toList
  }

  private def createEmployee(firstName: String, lastName: String, projectName: String): Employee = {
    val employee = new Employee
    employee.setFirstName(firstName)
    employee.setLastName(lastName)
    val project = employee.addProject(Project(projectName))
    val tx = em.getTransaction
    tx.begin()
    em.persist(employee)
    em.persist(project)
    tx.commit()
    employee
  }

  private def loadGroup(attributes: String*): LoadGroup = {
    val lg = new LoadGroup()
    attributes.foreach(lg.addAttribute)
    lg
  }

  private def assertLoaded(obj: AnyRef, attributeName: String): Unit = assertLoadedOrNot(obj, attributeName, true)

  private def assertNotLoaded(obj: AnyRef, attributeName: String): Unit = assertLoadedOrNot(obj, attributeName, false)

  private def assertLoadedOrNot(obj: AnyRef, attributeName: String, loaded: Boolean): Unit = {
    val message = s"$attributeName property should be ${if (loaded) "eagerly" else "lazy"} loaded"
    assert(pUtil.isLoaded(obj, attributeName) == loaded, s"because $message")
  }

}

case class QueryStatistics(readAllQuery: Int, readObjectQuery: Int, cacheHits: Int) {

  def numberOfDbQueries: Int = (readObjectQuery + readAllQuery) - cacheHits

  def diff(other: QueryStatistics): QueryStatistics =
    QueryStatistics(
      readAllQuery - other.readAllQuery,
      readObjectQuery - other.readObjectQuery,
      cacheHits - other.cacheHits
    )
}

object QueryStatistics {
  def nullToZero(a: Any): Int = Option(a).map(_.toString.toInt).getOrElse(0)

  def apply(pm: PerformanceMonitor): QueryStatistics =
    new QueryStatistics(
      nullToZero(pm.getOperationTimings.get("Counter:ReadAllQuery")),
      nullToZero(pm.getOperationTimings.get("Counter:ReadObjectQuery")),
      nullToZero(pm.getOperationTimings.get(SessionProfiler.CacheHits))
    )
}
