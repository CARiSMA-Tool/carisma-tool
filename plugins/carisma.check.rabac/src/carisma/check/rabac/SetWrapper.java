package carisma.check.rabac;

import java.util.HashSet;

class SetWrapper {
	private HashSet<String> set = new HashSet<>();

	public HashSet<String> getSet() {
		return this.set;
	}

	public void setSet(HashSet<String> set) {
		this.set = set;
	}

}