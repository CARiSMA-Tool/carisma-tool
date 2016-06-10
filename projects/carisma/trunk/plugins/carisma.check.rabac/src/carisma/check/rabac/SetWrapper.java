package carisma.check.rabac;

import java.util.HashSet;

class SetWrapper {
	private HashSet<String> set = new HashSet<String>();

	public HashSet<String> getSet() {
		return set;
	}

	public void setSet(HashSet<String> set) {
		this.set = set;
	}

}