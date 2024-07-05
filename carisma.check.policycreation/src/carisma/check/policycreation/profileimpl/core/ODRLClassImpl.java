package carisma.check.policycreation.profileimpl.core;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
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

	protected UMLModelConverter getHandler() {//TODO hidden for testing
		return handler;
	}
	public void setHandler(UMLModelConverter handler) {
		this.handler = handler;
	}




	public String getType() {//for the automatic JSON-Conversion currently used for testing
		return this.getClass().getSimpleName();
	}


	public Object createMap(Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		if (circlePreventionSet.contains(this)) {
			//TODO: Message: Error in conversion to JSON: cyclic references (Possibly instead change to reference elements by their id)
		}
		circlePreventionSet.add(this);
		Map<String,Object> newMap = new HashMap<>();
		//fillType(newMap);
		
		fillMapSimple(newMap, circlePreventionSet);
		Object possiblyNewObject = fillMapIndividual(newMap, circlePreventionSet);
		circlePreventionSet.remove(this);
		return possiblyNewObject != null ? possiblyNewObject : !(newMap.isEmpty()) ? newMap : null;//Determine cases where an Object is not displayed as map in the fillMapIndividual-method
	}
	
	public void fillMapSimple(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		Class<?> klass = this.getClass();
		while (ODRLClassImpl.class.isAssignableFrom(klass)) {
			for (Field f : klass.getDeclaredFields()) {
				Object termKey = handler.getTermMap().get(f);
				if (termKey instanceof String termKeyString) {
					try {//TODO decide how to handle the exception					
						Object termValue = klass.getMethod("get" + f.getName().substring(0, 1).toUpperCase() + f.getName().substring(1)).invoke(this);
						//Object termValue = f.get(this); //Does not work. Method is called as ODRLClassImpl even when called from subclass, hiding non-public fields from the subclass
						Object mapValue = handler.createMap(termValue, circlePreventionSet);
						if (mapValue != null &&
								( !(mapValue instanceof List<?> list) || !(list.isEmpty()) )) {
							map.put(termKeyString, mapValue);
						}
					} catch (IllegalArgumentException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IllegalAccessException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvocationTargetException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (NoSuchMethodException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
			klass=klass.getSuperclass();
			
		}
	}
	
	//Returns null if the map should be used as value for this object. If another value should be used, returns this value 
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return null;
	}
//	public void fillType(Map<String,Object> map) {
//		map.put("@type", handler.getTermMap().get(this.getClass()));//TODO
//	}
	
	//renamed from get to remove them form the testing with automated conversion
	public String gatClassTerm() {
		return handler.getTermMap().get(this.getClass());
	}
	public String getTypeKeyword() {
		return "@type";
	}
	public String gatIdKeyword() {
		return "@id";
	}
	public String getFieldTerm(String fieldName) throws NoSuchFieldException, SecurityException {
		return getFieldTerm(fieldName,this.getClass());
	}
	
	
	public String getFieldTerm(String fieldName, Class<?> klass) throws NoSuchFieldException, SecurityException {//TODO: Possibly change TermMapping from directly mapping classes and features to a 2-level-map that operates on the class as key at the first level and on the feature name as key at the 2nd level
		if (!(ODRLClassImpl.class.isAssignableFrom(klass))) {
			throw new NoSuchFieldException(fieldName);
		}
		try {
			return (handler.getTermMap().get(klass.getDeclaredField(fieldName)));
		} catch (NoSuchFieldException e) {
			Class<?> superKlass = klass.getSuperclass();
			if (ODRLClassImpl.class.isAssignableFrom(superKlass)) {
				return getFieldTerm(fieldName, superKlass);
			} else {
				throw e;
			}
		}
	}
	
}
