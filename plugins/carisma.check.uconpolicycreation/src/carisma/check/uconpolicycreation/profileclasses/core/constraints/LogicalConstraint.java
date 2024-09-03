package carisma.check.uconpolicycreation.profileclasses.core.constraints;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

import carisma.check.uconpolicycreation.UMLModelConverter;
import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.operand.Operand;

public class LogicalConstraint extends ODRLClass implements ConstraintInterface{
	String uid;
	Operand operand;
	
	
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
	
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getLogicalConstraint_LogicalOperator());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof Operand operand) {
				this.setOperand(operand);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getLogicalConstraint_Constraints());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Constraint> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, Constraint.class);
			if (attributeValueOdrl!=null && this.getOperand()!=null) {//TODO Maybe remove operand-nullcheck, as it being null would point to a faulty model
				this.getOperand().setConstraints(attributeValueOdrl);//(After creation of operand earlier in this method) set constraints to it
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getLogicalConstraint_Uid());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {
			this.setUid(stringValue);
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		Object operatorEntry = handler.createMap(operand, circlePreventionSet);
		map.put(operand.gatClassTerm(), operatorEntry);
		return null;
	}
	
}
