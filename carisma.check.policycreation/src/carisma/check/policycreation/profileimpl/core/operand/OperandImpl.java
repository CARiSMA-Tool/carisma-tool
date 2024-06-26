package carisma.check.policycreation.profileimpl.core.operand;

import java.util.List;

import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.constraint.ConstraintImpl;

public abstract class OperandImpl extends ODRLClassImpl {
	List<ConstraintImpl> constraints;
	
	String constraintsName;//TODO Handle differently as Operand-subproperties

	public List<ConstraintImpl> getConstraints() {
		return constraints;
	}

	public void setConstraints(List<ConstraintImpl> constraints) {
		this.constraints = constraints;
	}
	
	
}
