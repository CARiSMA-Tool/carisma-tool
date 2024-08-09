package carisma.check.policycreation.profileimpl.core.constraint;

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClass;
import carisma.check.policycreation.profileimpl.core.leftoperand.LeftOperand;
import carisma.check.policycreation.profileimpl.core.operator.Operator;

public class Constraint extends ODRLClass{
	String uid;
	LeftOperand leftOperand;
	Operator operator;
	List<String> rightOperand;
	List<String> rightOperandReference;
	String dataType;
	String unit;
	String status;

	
	
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





	public List<String> getRightOperand() {
		return rightOperand;
	}





	public void setRightOperand(List<String> rightOperand) {
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
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_DataType());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {		
			this.setDataType(stringValue);
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_LeftOperand());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof LeftOperand leftOperand) {
				this.setLeftOperand(leftOperand);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_Operator());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof Operator operator) {
				this.setOperator(operator);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_RightOperand());
		if (attributeValue instanceof List list) { //TODO List attribute, also rightOperand not yet implemented
			List<String> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, String.class);
			if (attributeValueOdrl!=null) {
				this.setRightOperand(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_RightOperandReference());
		if (attributeValue instanceof List list) { //TODO List attribute, also rightOperand not yet implemented
			List<String> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, String.class);
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
