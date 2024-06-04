package carisma.profile.uconcreation.odrl.core.internal.classes.constraint;

import java.util.List;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.leftoperand.LeftOperand;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.Operator;
import carisma.profile.uconcreation.odrl.core.internal.classes.rightoperand.RightOperandInterface;

public class Constraint extends ODRLClass implements ConstraintInterface{
	String uid;
	LeftOperand leftOperand;
	Operator operator;
	List<RightOperandInterface> rightOperand;
	List<String> rightOperandReference;
	String dataType;
	String unit;
	String status;
	
	String uidName;
	String leftOperandName;
	String operatorName;
	String rightOperandName;
	String rightOperandReferenceName;
	String dataTypeName;
	String unitName;
	String statusName;

}
