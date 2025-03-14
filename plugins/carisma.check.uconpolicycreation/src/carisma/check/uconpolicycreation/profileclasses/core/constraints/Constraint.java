package carisma.check.uconpolicycreation.profileclasses.core.constraints;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

import carisma.check.uconpolicycreation.UMLModelConverter;
import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.leftoperand.LeftOperand;
import carisma.check.uconpolicycreation.profileclasses.core.operator.Operator;

public class Constraint extends ODRLClass{
	/**
	 * Reference to an external Constraint.
	 */
	String uid;
	LeftOperand leftOperand;
	Operator operator;
	List<String> rightOperand = new LinkedList<>();
	List<String> rightOperandReference = new LinkedList<>();
	/**
	 * The Datatype which the contents of the attributes {@link #rightOperand} and {@link #rightOperandReference} are interpreted as.
	 */
	String dataType;
	/**
	 * The Unit which the contents of the attributes {@link #rightOperand} and {@link #rightOperandReference} are interpreted in.
	 */
	String unit;
	/**
	 * A pre-existing status reated to the {@link leftOperand}.
	 */
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
		if (attributeValue instanceof List<?> list) { //rightOperand not yet implemented
			List<String> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, String.class);
			if (attributeValueOdrl!=null) {
				this.setRightOperand(attributeValueOdrl);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getConstraint_RightOperandReference());
		if (attributeValue instanceof List<?> list) { //rightOperand not yet implemented
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





	@Override
	public Object fillMapIndividual(Map<String, Object> map, Set<ODRLClass> circlePreventionSet)
			throws NoSuchFieldException, SecurityException {
		String typeValue = handler.createMap(dataType, circlePreventionSet);
		if (typeValue!=null) {
			if (rightOperand.size()>1) {
				List<Map<String,String>> roList = new LinkedList<Map<String,String>>();
				map.put("rightOperand", roList);
				for (String rightOp : rightOperand) {
					if (rightOp != null) {
						Map<String,String> roMap = new HashMap<>();
						roMap.put("@value", rightOp);
						roMap.put(getTypeKeyword(), typeValue);
					}
				}
			} else if (rightOperand.size()==1) {
				Map<String,String> roMap = new HashMap<>();
				roMap.put("@value", rightOperand.get(0));
				roMap.put(getTypeKeyword(), typeValue);
				map.put("rightOperand", roMap);
			}
			if (rightOperandReference.size()>1) {
				List<Map<String,String>> rorList = new LinkedList<Map<String,String>>();
				map.put("rightOperandReference", rorList);
				for (String rightOpR : rightOperandReference) {
					if (rightOpR != null) {
						Map<String,String> rorMap = new HashMap<>();
						rorMap.put("@value", rightOpR);
						rorMap.put(getTypeKeyword(), typeValue);
					}
				}
			} else if (rightOperandReference.size()==1) {
				Map<String,String> rorMap = new HashMap<>();
				rorMap.put("@value", rightOperand.get(0));
				rorMap.put(getTypeKeyword(), typeValue);
				map.put("rightOperandReference", rorMap);
			}
			
		}
		//part of something not implemented
//		if (this.uid!=null && map.size()!=1) {
//			//Display externally if it defines a new element referencable from outside through uid
//			map.put(gatTypeKeyword(), handler.getTermMap().get(this.getClass()));
//			map.put("@context", handler.getContextMapValue());
//			handler.addToTopLevelMapElements(map);
//			Map<String,Object> referenceToConstraint = new HashMap<>();
//			referenceToConstraint.put(gatIdKeyword(), this.uid);
//			return referenceToConstraint;
//		}
		return null;
	}
	
	


}
