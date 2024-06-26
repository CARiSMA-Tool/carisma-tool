package carisma.profile.uconcreation.odrl.core.internal.classes.action;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;

public interface Action extends ODRLClass {
	
	
	public ConstraintInterface getRefinement();




	public void setRefinement(ConstraintInterface refinement);




}
