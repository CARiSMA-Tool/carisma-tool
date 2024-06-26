package carisma.profile.uconcreation.odrl.core.internal.classes.failure;

import java.util.List;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Rule;

public interface Failure extends ODRLClass {

	public List<Rule> getRules();

	public void setRules(List<Rule> rules);
	
	
}
