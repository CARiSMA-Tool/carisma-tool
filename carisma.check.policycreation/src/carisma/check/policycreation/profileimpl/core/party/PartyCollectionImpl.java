package carisma.check.policycreation.profileimpl.core.party;

import org.eclipse.emf.ecore.EObject;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.constraint.ConstraintInterfaceImpl;

public class PartyCollectionImpl extends PartyImpl {
	ConstraintInterfaceImpl refinement;
	String source;
	
	String refinementName;
	String sourceName;
	
	
	public ConstraintInterfaceImpl getRefinement() {
		return refinement;
	}
	public void setRefinement(ConstraintInterfaceImpl refinement) {
		this.refinement = refinement;
	}
	public String getSource() {
		return source;
	}
	public void setSource(String source) {
		this.source = source;
	}
	
	
	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getPartyCollection_Source());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {
			this.setSource(stringValue);
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRefinableElement_Refinement()); 
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
			if (attributeValueOdrl instanceof ConstraintInterfaceImpl refinement) {
				this.setRefinement(refinement);
			}
		}
	}
	
}
