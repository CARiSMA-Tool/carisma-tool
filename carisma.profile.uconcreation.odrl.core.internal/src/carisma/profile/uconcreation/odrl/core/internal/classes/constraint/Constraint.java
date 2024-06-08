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

	
	
	
	
	public String getUid() {
		return uid;
	}





	public void setUid(String uid) {
		this.uid = uid;
	}





	public LeftOperand getLeftOperand() {
		return leftOperand;
	}





	public void setLeftOperand(LeftOperand leftOperand) {
		this.leftOperand = leftOperand;
	}





	public Operator getOperator() {
		return operator;
	}





	public void setOperator(Operator operator) {
		this.operator = operator;
	}





	public List<RightOperandInterface> getRightOperand() {
		return rightOperand;
	}





	public void setRightOperand(List<RightOperandInterface> rightOperand) {
		this.rightOperand = rightOperand;
	}





	public List<String> getRightOperandReference() {
		return rightOperandReference;
	}





	public void setRightOperandReference(List<String> rightOperandReference) {
		this.rightOperandReference = rightOperandReference;
	}





	public String getDataType() {
		return dataType;
	}





	public void setDataType(String dataType) {
		this.dataType = dataType;
	}





	public String getUnit() {
		return unit;
	}





	public void setUnit(String unit) {
		this.unit = unit;
	}





	public String getStatus() {
		return status;
	}





	public void setStatus(String status) {
		this.status = status;
	}





	public String toString() {
		String returnString = this.getClass().getName();
		returnString += System.lineSeparator()+ uid.indent(stringIndent);
		returnString += System.lineSeparator()+ leftOperand.toString().indent(stringIndent);
		//......
		return returnString;
	}
}
