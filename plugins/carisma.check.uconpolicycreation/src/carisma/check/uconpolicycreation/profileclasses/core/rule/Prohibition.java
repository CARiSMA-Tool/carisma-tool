package carisma.check.uconpolicycreation.profileclasses.core.rule;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.uml2.uml.Element;

import carisma.check.uconpolicycreation.UMLModelConverter;
import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.failure.Remedy;
import carisma.check.uconpolicycreation.profileclasses.core.policy.Policy;

public class Prohibition extends Rule {
	/**
	 * What has to be done if this prohibition is not followed
	 */
	Remedy remedy;


	public Remedy getRemedy() {
		return remedy;
	}

	public void setRemedy(Remedy remedy) {
		this.remedy = remedy;
	}
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		EStructuralFeature remedyFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getProhibition_Remedies().getName());
		if (UMLModelConverter.getValue(currentEObject,odrlPackage.getProhibition_Remedies()) != null) {
			Object attributeValueOdrl = handler.addElement(remedyFeature, this, activityElement);
			if (attributeValueOdrl instanceof Remedy newRemedy) {
				this.setRemedy(newRemedy);
			}
		}// only set the remedy if its rules-Property is not empty (in ecore it has the empty list, making the remedy and List non-null in any case)
		Object attributeValue = UMLModelConverter.getValue(currentEObject,odrlPackage.getProhibition_Remedies());
		if (attributeValue instanceof List<?> list) {
			List<Duty> attributeValueOdrl = handler.addElement(list, this.getRemedy(), activityElement, Duty.class);
			if (attributeValueOdrl!=null) {
				this.getRemedy().getRules().addAll(attributeValueOdrl);//TODO change getters to conditional generators or add null-checks with additional creation everywhere were gotten objects are further used
				//remove duties from policy as direct elements, only contained through this rule
				for (Duty duty : attributeValueOdrl) {
					List<Policy> referringPolicies = new LinkedList<>();
					for (ODRLClass referringObject: duty.getReferredBy()) {
						if (referringObject instanceof Policy policy) {
							policy.getObligation().remove(duty);
							referringPolicies.add(policy);
						}
					}
					for (Policy policy : referringPolicies) {
						duty.removeReferredBy(policy);
					}
				}
			}
		}
	}
}
