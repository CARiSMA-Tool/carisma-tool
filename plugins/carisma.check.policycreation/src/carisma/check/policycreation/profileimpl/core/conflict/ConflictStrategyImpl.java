package carisma.check.policycreation.profileimpl.core.conflict;

import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;

public abstract class ConflictStrategyImpl extends ODRLClassImpl {
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return gatClassTerm();
	}
}
