package carisma.check.policycreation.profileimpl.core.action;

import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.constraint.ConstraintInterfaceImpl;

public abstract class ActionImpl extends ODRLClassImpl {
	ConstraintInterfaceImpl refinement;
	
	
	String refinementName;
	
	
	
	
	public ConstraintInterfaceImpl getRefinement() {
		return refinement;
	}




	public void setRefinement(ConstraintInterfaceImpl refinement) {
		this.refinement = refinement;
	}




}
