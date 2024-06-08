package carisma.profile.uconcreation.odrl.core.internal.classes.action;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;

public abstract class Action extends ODRLClass {
	ConstraintInterface refinement;
	
	
	String refinementName;
	
	
	
	
	public ConstraintInterface getRefinement() {
		return refinement;
	}




	public void setRefinement(ConstraintInterface refinement) {
		this.refinement = refinement;
	}




	public String toString() {
		String returnString = this.getClass().getName();
		returnString += System.lineSeparator() + refinement.toString().indent(stringIndent);
		return returnString;
	}
}
