package carisma.check.policycreation.profileimpl.core.rule;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.uml2.uml.Element;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.failure.Consequence;

public class Duty extends Rule {
	Consequence consequences;

	public Consequence getConsequences() {
		return consequences;
	}

	public void setConsequences(Consequence consequences) {
		this.consequences = consequences;
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
		//Following part currently leads to stack overflow when JSON-Objects are created (Does not anymore. did with old references)
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getDuty_Consequences());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<Duty> attributeValueOdrl = handler.addElement(list, this.getConsequences(), activityElement, Duty.class);
			if (attributeValueOdrl!=null) {
				if (this.getConsequences().getRules()==null)
					this.getConsequences().setRules(new LinkedList<Rule>());
				this.getConsequences().getRules().addAll(attributeValueOdrl);//TODO change getters to conditional generators or add null-checks with additional creation everywhere were gotten objects are further used
			}
		}
	}
}
