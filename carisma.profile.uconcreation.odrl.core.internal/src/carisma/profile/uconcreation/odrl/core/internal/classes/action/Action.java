package carisma.profile.uconcreation.odrl.core.internal.classes.action;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;

public abstract class Action extends ODRLClass {
	ConstraintInterface refinement;
	
	
	String refinementName;
}
