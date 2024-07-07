package carisma.check.policycreation.profileimpl.core.party;

import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;

public class PartyImpl extends ODRLClassImpl {
	String uid;


	public String getUid() {
		return uid;
	}



	public void setUid(String uid) {
		this.uid = uid;
	}
	
	@Override
	public void fill(EObject currentEObject, Element activityElement) {
		super.fill(currentEObject, activityElement);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getParty_Uid());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {	
			this.setUid(stringValue);
		}
	}
	
	@Override
	public Object fillMapIndividual(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet)
			throws NoSuchFieldException, SecurityException {
		if (map.size()==1 && map.get(getFieldTerm("uid"))!=null) {
				return map.get(getFieldTerm("uid"));
		} else {
			map.put(gatTypeKeyword(), gatClassTerm());//TODO: Not sure if that's how it is in the specification
		}
		return null;
	}
}
