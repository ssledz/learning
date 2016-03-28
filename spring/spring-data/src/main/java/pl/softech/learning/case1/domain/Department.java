package pl.softech.learning.case1.domain;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToMany;

@Entity
public class Department {

	@Id
	@GeneratedValue
	private Integer id;

	private String name;

	@OneToMany(mappedBy = "department", orphanRemoval = true, fetch = FetchType.EAGER, cascade = { CascadeType.ALL })
	private Set<User> users = new HashSet<>();

	protected Department() {
	}

	public Department(final String name) {
		this.name = name;
	}

	public Set<User> getUsers() {
		return Collections.unmodifiableSet(users);
	}

	public void remove(final User user) {
		users.remove(user);
	}

	public String getName() {
		return name;
	}

	public void addUser(final User user) {
		user.setDepartment(this);
		users.add(user);
	}

}
