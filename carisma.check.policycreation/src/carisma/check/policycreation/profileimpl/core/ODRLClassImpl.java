package carisma.check.policycreation.profileimpl.core;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.ENamedElement;
import org.eclipse.emf.ecore.EObject;
import org.json.JSONObject;

import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import carisma.check.policycreation.UMLModelConverter;

public abstract class ODRLClassImpl{
	protected Set<ODRLClassImpl> referredBy;
	public ENamedElement containingUMLElement;
	public ODRLClassImpl directParent;//maybe
	protected ODRLCommonVocabularyPackage odrlPackage = UMLModelConverter.odrlPackage;
	public UMLModelConverter handler = null;
	

	
	
	public void fill(EObject input, EObject activityElement, UMLModelConverter handler) {
	}

	public UMLModelConverter getHandler() {
		return handler;
	}
	public void setHandler(UMLModelConverter handler) {
		this.handler = handler;
	}




	public String getType() {//for the automatic JSON-Conversion currently used for testing
		return this.getClass().getSimpleName();
	}


	public Object createMap(Set<ODRLClassImpl> circlePreventionSet) {
		if (circlePreventionSet.contains(this)) {
			//TODO: Message: Error in conversion to JSON: cyclic references (Possibly instead change to reference elements by their id)
		}
		circlePreventionSet.add(this);
		Map<String,Object> newMap = new HashMap<>();
		fillType(newMap);
		fillMap(newMap, circlePreventionSet);
		circlePreventionSet.remove(this);
		return newMap;
	}
	
	public void fillMap(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) {
		
	}
	public void fillType(Map<String,Object> map) {
		map.put("@type", handler.getTermMap().get(this.getClass()));
	}
	
}
