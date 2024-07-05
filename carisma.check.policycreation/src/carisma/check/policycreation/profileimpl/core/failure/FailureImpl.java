package carisma.check.policycreation.profileimpl.core.failure;

import java.util.List;

import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.rule.RuleImpl;

public abstract class FailureImpl extends ODRLClassImpl {
	List<RuleImpl> rules;

	public List<RuleImpl> getRules() {
		return rules;
	}

	public void setRules(List<RuleImpl> rules) {
		this.rules = rules;
	}
	
	
}
