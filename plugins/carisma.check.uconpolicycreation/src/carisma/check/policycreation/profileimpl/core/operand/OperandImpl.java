package carisma.check.policycreation.profileimpl.core.operand;

import java.util.List;
import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.constraint.ConstraintImpl;

public abstract class OperandImpl extends ODRLClassImpl {
	List<ConstraintImpl> constraints;


	public List<ConstraintImpl> getConstraints() {
		return constraints;
	}

	public void setConstraints(List<ConstraintImpl> constraints) {
		this.constraints = constraints;
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return map.get(getFieldTerm("constraints"));
	}
	
	
}
