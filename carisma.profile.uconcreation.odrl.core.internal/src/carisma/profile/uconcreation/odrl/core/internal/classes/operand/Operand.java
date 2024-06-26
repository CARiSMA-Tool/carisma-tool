package carisma.profile.uconcreation.odrl.core.internal.classes.operand;

import java.util.List;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.Constraint;

public interface Operand extends ODRLClass {

	public List<Constraint> getConstraints();

	public void setConstraints(List<Constraint> constraints);
	
	
}
