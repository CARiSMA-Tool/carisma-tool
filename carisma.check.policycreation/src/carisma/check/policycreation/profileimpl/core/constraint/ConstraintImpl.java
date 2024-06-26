package carisma.check.policycreation.profileimpl.core.constraint;

import java.util.List;

import org.eclipse.emf.ecore.EObject;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.leftoperand.LeftOperandImpl;
import carisma.check.policycreation.profileimpl.core.operator.OperatorImpl;
import carisma.check.policycreation.profileimpl.core.rightoperand.RightOperandInterfaceImpl;

public class ConstraintImpl extends ODRLClassImpl{
	String uid;
	LeftOperandImpl leftOperand;
	OperatorImpl operator;
	List<RightOperandInterfaceImpl> rightOperand;
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





	public LeftOperandImpl getLeftOperand() {
		return leftOperand;
	}





	public void setLeftOperand(LeftOperandImpl leftOperand) {
		this.leftOperand = leftOperand;
	}





	public OperatorImpl getOperator() {
		return operator;
	}





	public void setOperator(OperatorImpl operator) {
		this.operator = operator;
	}





	public List<RightOperandInterfaceImpl> getRightOperand() {
		return rightOperand;
	}





	public void setRightOperand(List<RightOperandInterfaceImpl> rightOperand) {
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


	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_DataType());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {		
			this.setDataType(stringValue);
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_LeftOperand());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
			if (attributeValueOdrl instanceof LeftOperandImpl leftOperand) {
				this.setLeftOperand(leftOperand);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_Operator());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
			if (attributeValueOdrl instanceof OperatorImpl operator) {
				this.setOperator(operator);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_RightOperand());
		if (attributeValue instanceof List list) { //TODO List attribute, also rightOperand not yet implemented
			List<RightOperandInterfaceImpl> attributeValueOdrl = handler.addElement(list, this, activityElement, RightOperandInterfaceImpl.class);
			if (attributeValueOdrl!=null) {
				this.setRightOperand(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_RightOperandReference());
		if (attributeValue instanceof List list) { //TODO List attribute, also rightOperand not yet implemented
			List<String> attributeValueOdrl = handler.addElement(list, this, activityElement, String.class);
			if (attributeValueOdrl!=null) {
				this.setRightOperandReference(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_Status());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {			
			this.setStatus(stringValue);
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_Uid());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {		
				this.setUid(stringValue);			
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_Unit());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {		
			this.setUnit(stringValue);
		}
	}


}
