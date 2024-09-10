package carisma.check.uconpolicycreation.profileclasses.core.rule;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

import carisma.check.uconpolicycreation.UMLModelConverter;
import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.policy.Policy;

public class Permission extends Rule {
	/**
	 * What has to be done for the right to exercise this permission
	 */
	List<Duty> duties =  new LinkedList<>();

	public List<Duty> getDuties() {
		return duties;
	}

	public void setDuties(List<Duty> duties) {
		this.duties = duties;
	}
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getPermission_Duties());
		if (attributeValue instanceof List<?> list) {
			List<Duty> attributeValueOdrl = handler.addElement(list, this, activityElement, Duty.class);
			if (attributeValueOdrl!=null) {
				this.setDuties(attributeValueOdrl);
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
