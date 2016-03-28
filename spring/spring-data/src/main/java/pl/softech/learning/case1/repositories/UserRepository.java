package pl.softech.learning.case1.repositories;

import java.util.List;

import org.springframework.data.repository.CrudRepository;

import pl.softech.learning.case1.domain.User;

public interface UserRepository extends CrudRepository<User, Long> {
	List<User> findByLastname(String lastname);
}
