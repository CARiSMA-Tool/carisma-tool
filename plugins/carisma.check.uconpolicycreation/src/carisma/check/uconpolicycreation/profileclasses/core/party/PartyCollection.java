package carisma.check.uconpolicycreation.profileclasses.core.party;

import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

import carisma.check.uconpolicycreation.UMLModelConverter;
import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.constraints.ConstraintInterface;

public class PartyCollection extends Party {
	ConstraintInterface refinement;
	String source;

	
	public ConstraintInterface getRefinement() {
		return refinement;
	}
	public void setRefinement(ConstraintInterface refinement) {
		this.refinement = refinement;
	}
	public String getSource() {
		return source;
	}
	public void setSource(String source) {
		this.source = source;
	}
	
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getPartyCollection_Source());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {
			this.setSource(stringValue);
		}
		attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getRefinableElement_Refinement()); 
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, containingUmlElement);
			if (attributeValueOdrl instanceof ConstraintInterface refinement) {
				this.setRefinement(refinement);
			}
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String, Object> map, Set<ODRLClass> circlePreventionSet)
			throws NoSuchFieldException, SecurityException {
		map.put(gatTypeKeyword(), gatClassTerm());
		return null;
	}
	
}
