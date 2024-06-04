package carisma.profile.uconcreation.odrl.core.internal.classes.failure;

import java.util.Set;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Rule;

public abstract class Failure extends ODRLClass {
	Set<Rule> rules;
	
	String rulesName;//TODO treat differently as subproperty
}
