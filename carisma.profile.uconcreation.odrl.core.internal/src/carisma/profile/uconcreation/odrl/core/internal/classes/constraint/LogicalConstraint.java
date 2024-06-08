package carisma.profile.uconcreation.odrl.core.internal.classes.constraint;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Operand;

public class LogicalConstraint extends ODRLClass implements ConstraintInterface{
	String uid;
	Operand operand;
	
	String uidName;
	String operandName;
	public String getUid() {
		return uid;
	}
	public void setUid(String uid) {
		this.uid = uid;
	}
	public Operand getOperand() {
		return operand;
	}
	public void setOperand(Operand operand) {
		this.operand = operand;
	}
	
	
	
}
