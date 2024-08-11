package carisma.check.policycreation.profileclasses.core.conflict;

import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileclasses.ODRLClass;

public abstract class ConflictStrategy extends ODRLClass {
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return gatClassTerm();
	}
}
