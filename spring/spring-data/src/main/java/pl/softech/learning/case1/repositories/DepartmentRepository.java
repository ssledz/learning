package pl.softech.learning.case1.repositories;

import org.springframework.data.repository.CrudRepository;

import pl.softech.learning.case1.domain.Department;

public interface DepartmentRepository extends CrudRepository<Department, Long> {

	Department findByName(String name);

	Department saveAndFlush(Department department);

}
