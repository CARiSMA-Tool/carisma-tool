package carisma.check.policycreation.profileimpl.core.operand;

import java.util.List;
import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileimpl.core.ODRLClass;
import carisma.check.policycreation.profileimpl.core.constraint.Constraint;

public abstract class Operand extends ODRLClass {
	List<Constraint> constraints;


	public List<Constraint> getConstraints() {
		return constraints;
	}

	public void setConstraints(List<Constraint> constraints) {
		this.constraints = constraints;
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return handler.createMap(constraints, circlePreventionSet);
	}
	
	
}
