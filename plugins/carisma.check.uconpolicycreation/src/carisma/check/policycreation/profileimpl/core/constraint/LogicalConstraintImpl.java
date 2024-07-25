package carisma.check.policycreation.profileimpl.core.constraint;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.operand.OperandImpl;

public class LogicalConstraintImpl extends ODRLClassImpl implements ConstraintInterfaceImpl{
	String uid;
	OperandImpl operand;
	
	
	public String getUid() {
		return uid;
	}
	public void setUid(String uid) {
		this.uid = uid;
	}
	public OperandImpl getOperand() {
		return operand;
	}
	public void setOperand(OperandImpl operand) {
		this.operand = operand;
	}
	
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getLogicalConstraint_LogicalOperator());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof OperandImpl operand) {
				this.setOperand(operand);
			}
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getLogicalConstraint_Constraints());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<ConstraintImpl> attributeValueOdrl = handler.addElement(list, this, containingUmlElement, ConstraintImpl.class);
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
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		Object operatorEntry = handler.createMap(operand, circlePreventionSet);
		map.put(operand.gatClassTerm(), operatorEntry);
		return null;
	}
	
}
