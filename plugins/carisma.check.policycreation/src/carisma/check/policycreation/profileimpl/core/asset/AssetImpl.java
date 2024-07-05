package carisma.check.policycreation.profileimpl.core.asset;

import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;

public class AssetImpl extends ODRLClassImpl {
	String uid;
	
	
	
	
	public String getUid() {
		return uid;
	}




	public void setUid(String uid) {
		this.uid = uid;
	}
	
	@Override
	public void fill(EObject currentEObject, EObject activityElement, UMLModelConverter handler) {
		super.fill(currentEObject, activityElement, handler);
		Object attributeValue = UMLModelConverter.getValue(currentEObject, odrlPackage.getAsset_Uid());
		if (attributeValue instanceof String stringValue && !stringValue.isEmpty()) {			
			this.setUid(stringValue);
		}
	}




	@Override
	public Object fillMapIndividual(Map<String, Object> map, Set<ODRLClassImpl> circlePreventionSet)
			throws NoSuchFieldException, SecurityException {
		System.out.println("Class: . : " + this.getClass());
		System.out.println("Feature: " + this.getClass().getDeclaredField("uid"));
		if (map.size()==1 && map.get(getFieldTerm("uid"))!=null) {
				return map.get(getFieldTerm("uid"));
		} else {
			map.put(getTypeKeyword(), gatClassTerm());//TODO: Not sure if that's how it is in the specification
		}
		return null;
	}

	
	
}
