package carisma.check.uconpolicycreation.profileclasses.core.rule;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.uml2.uml.Element;

import carisma.check.uconpolicycreation.UMLModelConverter;
import carisma.check.uconpolicycreation.profileclasses.core.failure.Consequence;

public class Duty extends Rule {
	/**
	 * What has to be done if this duty is not exercised
	 */
	Consequence consequence;

	public Consequence getConsequence() {
		return consequence;
	}

	public void setConsequences(Consequence consequences) {
		this.consequence = consequences;
	}
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		EStructuralFeature consequenceFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getDuty_Consequences().getName());
		if (UMLModelConverter.getValue(currentEObject,odrlPackage.getDuty_Consequences()) != null) {
			Object attributeValueOdrl = handler.addElement(consequenceFeature, this, activityElement);
			if (attributeValueOdrl instanceof Consequence consequence) {
				this.setConsequences(consequence);
			}
		}
		
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getDuty_Consequences());
		if (attributeValue instanceof List list) {
			List<Duty> attributeValueOdrl = handler.addElement(list, this.getConsequence(), activityElement, Duty.class);
			if (attributeValueOdrl!=null) {
				if (this.getConsequence().getRules()==null)
					this.getConsequence().setRules(new LinkedList<>());
				this.getConsequence().getRules().addAll(attributeValueOdrl);
			}
		}
	}
}
