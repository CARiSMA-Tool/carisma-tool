package carisma.profile.uconcreation.odrl.core.internal.classes.constraint;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Operand;

public interface LogicalConstraint extends ODRLClass, ConstraintInterface{
	
	public String getUid();
	public void setUid(String uid);
	public Operand getOperand();
	public void setOperand(Operand operand);
	
	
	
}
