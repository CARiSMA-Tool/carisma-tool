package carisma.check.policycreation.profileimpl.core.rule;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.failure.ConsequenceImpl;

public class DutyImpl extends RuleImpl {
	ConsequenceImpl consequences;
	
	String consequencesName;

	public ConsequenceImpl getConsequences() {
		return consequences;
	}

	public void setConsequences(ConsequenceImpl consequences) {
		this.consequences = consequences;
	}
	
	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		EStructuralFeature consequenceFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getDuty_Consequences().getName());
		if (UMLModelConverter.getValue(currentEObject,odrlPackage.getDuty_Consequences()) != null) {
			System.out.println("ConsequenceValue" + UMLModelConverter.getValue(currentEObject,odrlPackage.getDuty_Consequences()));
			Object attributeValueOdrl = handler.addElement(consequenceFeature, this, activityElement);
			if (attributeValueOdrl instanceof ConsequenceImpl consequence) {
				this.setConsequences(consequence);
			}
		}
		//Following part currently leads to stack overflow when JSON-Objects are created (Does not anymore. did with old references)
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getDuty_Consequences());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<DutyImpl> attributeValueOdrl = handler.addElement(list, this.getConsequences(), activityElement, DutyImpl.class);
			if (attributeValueOdrl!=null) {
				if (this.getConsequences().getRules()==null)
					this.getConsequences().setRules(new LinkedList<RuleImpl>());
				this.getConsequences().getRules().addAll(attributeValueOdrl);//TODO change getters to conditional generators or add null-checks with additional creation everywhere were gotten objects are further used
			}
		}
	}
}
