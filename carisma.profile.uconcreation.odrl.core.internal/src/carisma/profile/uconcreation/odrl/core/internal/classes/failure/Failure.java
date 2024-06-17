package carisma.profile.uconcreation.odrl.core.internal.classes.failure;

import java.util.List;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Rule;

public abstract class Failure extends ODRLClass {
	List<Rule> rules;
	
	String rulesName;//TODO treat differently as subproperty

	public List<Rule> getRules() {
		return rules;
	}

	public void setRules(List<Rule> rules) {
		this.rules = rules;
	}
	
	
}
