package carisma.profile.uconcreation.odrl.core.internal.classes.constraint;

import java.util.List;

import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.leftoperand.LeftOperand;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.Operator;
import carisma.profile.uconcreation.odrl.core.internal.classes.rightoperand.RightOperandInterface;

public interface Constraint extends ODRLClass{

	
	
	
	
	public String getUid();





	public void setUid(String uid);





	public LeftOperand getLeftOperand();





	public void setLeftOperand(LeftOperand leftOperand);





	public Operator getOperator();





	public void setOperator(Operator operator);




	public List<RightOperandInterface> getRightOperand();





	public void setRightOperand(List<RightOperandInterface> rightOperand);





	public List<String> getRightOperandReference();





	public void setRightOperandReference(List<String> rightOperandReference);





	public String getDataType();





	public void setDataType(String dataType);





	public String getUnit();





	public void setUnit(String unit);





	public String getStatus();





	public void setStatus(String status);


}
