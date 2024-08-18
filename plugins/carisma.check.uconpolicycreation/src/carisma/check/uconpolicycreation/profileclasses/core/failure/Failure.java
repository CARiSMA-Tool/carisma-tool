package carisma.check.uconpolicycreation.profileclasses.core.failure;

import java.util.LinkedList;
import java.util.List;

import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Rule;

public abstract class Failure extends ODRLClass {
	List<Rule> rules = new LinkedList<>();

	public List<Rule> getRules() {
		return rules;
	}

	public void setRules(List<Rule> rules) {
		this.rules = rules;
	}
	
	
}
