package carisma.check.policycreation.profileimpl.core.function;

import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

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
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getPartyFunction_Party());
		if (attributeValue instanceof EObject newEObj) {
			Object attributeValueOdrl = handler.addElement(newEObj, this, baseElement);
			if (attributeValueOdrl instanceof PartyImpl partyImpl) {
				this.setParty(partyImpl);
			}
		}		
	}
}
