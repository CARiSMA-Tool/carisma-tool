package carisma.check.policycreation.profileimpl.core.rule;

import java.util.List;

import org.eclipse.emf.ecore.EObject;

import carisma.check.policycreation.UMLModelConverter;

public class PermissionImpl extends RuleImpl {
	List<DutyImpl> duties;

	public List<DutyImpl> getDuties() {
		return duties;
	}

	public void setDuties(List<DutyImpl> duties) {
		this.duties = duties;
	}
	
	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getPermission_Duties());
		if (attributeValue instanceof List list) { //TODO List attribute
			List<DutyImpl> attributeValueOdrl = handler.addElement(list, this, activityElement, DutyImpl.class);
			if (attributeValueOdrl!=null) {
				this.setDuties(attributeValueOdrl);
			}
		}
	}
}
