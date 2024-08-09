package carisma.check.policycreation.profileimpl.core.failure;

import java.util.List;

import carisma.check.policycreation.profileimpl.core.ODRLClass;
import carisma.check.policycreation.profileimpl.core.rule.Rule;

public abstract class Failure extends ODRLClass {
	List<Rule> rules;

	public List<Rule> getRules() {
		return rules;
	}

	public void setRules(List<Rule> rules) {
		this.rules = rules;
	}
	
	
}
