package carisma.check.policycreation.profileimpl.core.function;

import org.eclipse.emf.ecore.EObject;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.party.PartyImpl;

public class FunctionImpl extends ODRLClassImpl {
	PartyImpl party;

	public PartyImpl getParty() {
		return party;
	}

	public void setParty(PartyImpl party) {
		this.party = party;
	}
	
	
	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getPartyFunction_Party());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, activityElement);
			if (attributeValueOdrl instanceof PartyImpl party) {
				this.setParty(party);
			}
		}
		
	}
	
}
