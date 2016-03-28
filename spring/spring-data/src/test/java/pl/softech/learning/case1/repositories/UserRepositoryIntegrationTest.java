package pl.softech.learning.case1.repositories;

import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;

import pl.softech.learning.case1.domain.Department;
import pl.softech.learning.case1.domain.User;
import pl.softech.learning.case1.repositories.DepartmentRepository;
import pl.softech.learning.case1.repositories.UserRepository;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = Case1TestConfig.class)
public class UserRepositoryIntegrationTest {

	@Autowired
	private UserRepository repository;
	@Autowired
	private DepartmentRepository departmentRepository;

	@PersistenceContext
	private EntityManager em;

	@Test
	@Transactional
	public void sampleTestCase() {

		User dave = new User("Dave", "Matthews");
		dave = repository.save(dave);

		User carter = new User("Carter", "Beauford");
		carter = repository.save(carter);

		final List<User> result = repository.findByLastname("Matthews");

		Assert.assertEquals(1, result.size());
		Assert.assertEquals(dave, result.get(0));

	}

	@Test
	@Transactional
	public void departmentTestCase() {

		Department department = new Department("IT");
		final User dave = new User("Dave", "Dave");
		final User tom = new User("Tom", "Tom");
		final User cris = new User("Cris", "Cris");

		department.addUser(dave);
		department.addUser(tom);
		department.addUser(cris);

		departmentRepository.saveAndFlush(department);

		em.clear();

		department = departmentRepository.findByName("IT");
		Assert.assertEquals(3, department.getUsers().size());
		department.remove(repository.findOne(dave.getId()));
		departmentRepository.saveAndFlush(department);

		em.clear();

		department = departmentRepository.findByName("IT");
		Assert.assertEquals(2, department.getUsers().size());
		Assert.assertEquals(0, repository.findByLastname("Dave").size());

	}
}
