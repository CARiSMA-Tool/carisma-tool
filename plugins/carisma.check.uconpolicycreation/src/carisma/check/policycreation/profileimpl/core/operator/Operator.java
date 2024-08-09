package carisma.check.policycreation.profileimpl.core.operator;

import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileimpl.core.ODRLClass;

public abstract class Operator extends ODRLClass {
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return gatClassTerm();
	}
}
