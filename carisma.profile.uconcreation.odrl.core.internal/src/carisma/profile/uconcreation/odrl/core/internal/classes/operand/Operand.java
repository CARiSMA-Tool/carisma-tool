package carisma.profile.uconcreation.odrl.core.internal.classes.operand;

import java.util.List;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.Constraint;

public abstract class Operand extends ODRLClass {
	List<Constraint> constraints;
	
	String constraintsName;//TODO Handle differently as Operand-subproperties
	
}
