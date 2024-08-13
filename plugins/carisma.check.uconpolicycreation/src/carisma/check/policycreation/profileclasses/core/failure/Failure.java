package carisma.check.policycreation.profileclasses.core.failure;

import java.util.LinkedList;
import java.util.List;

import carisma.check.policycreation.profileclasses.ODRLClass;
import carisma.check.policycreation.profileclasses.core.rule.Rule;

public abstract class Failure extends ODRLClass {
	List<Rule> rules = new LinkedList<>();

	public List<Rule> getRules() {
		return rules;
	}

	public void setRules(List<Rule> rules) {
		this.rules = rules;
	}
	
	
}
