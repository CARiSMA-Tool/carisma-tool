package carisma.check.uconpolicycreation.profileclasses;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.util.UMLUtil;

import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import carisma.check.uconpolicycreation.UMLModelConverter;
/**
 * Top-Level-Class for representing elements of ODRL profiles. Concrete elements are represented by Classes inheriting from this Class.
 */
public abstract class ODRLClass{
	/**
	 * ODRLObjects that contain a reference to this object.
	 */
	protected List<ODRLClass> referredBy = new LinkedList<>();
	//public ODRLClass directParent;//maybe use for better location in messages
	/**
	 * Package generated from the used profile.
	 */
	protected ODRLCommonVocabularyPackage odrlPackage = UMLModelConverter.odrlPackage;
	/**
	 * Handles the conversion of this object.
	 */
	protected UMLModelConverter handler = null;
	/**
	 * UML-diagram-element containing the element corresponding to this Object.
	 */
	protected Element containingUmlElement;
	
	
	public List<ODRLClass> getReferredBy() {
		return referredBy;
	}
	public void addReferredBy(ODRLClass parent) {
		referredBy.add(parent);
	}
	public void removeReferredBy(ODRLClass parent) {
		referredBy.remove(parent);
	}
	

	/**
	 * Fills the attributes of this object.
	 * @param input {@link EObject} from which the values are taken
	 * @param activityElement {@link Element} containing this object's model-representation
	 */
	public void fill(EObject input, Element activityElement) {
		if (containingUmlElement == null) {
			containingUmlElement = UMLUtil.getBaseElement(input);
			if (containingUmlElement == null) {
				containingUmlElement = activityElement;
			} else {//New applied Stereotype was processed
				handler.addToReferencingMap(input, this);
			}
		}
	}

	public UMLModelConverter getHandler() {
		return handler;
	}
	public void setHandler(UMLModelConverter handler) {
		this.handler = handler;
		handler.addToHandledOdrlObjects(this);
	}
	

	public Element gatContainingUmlElement() {
		return containingUmlElement;
	}

	public void setContainingUmlElement(Element baseElement) {
		this.containingUmlElement = baseElement;
	}


	
	/**
	 * Creates a JSON-LD-structured map from this object.
	 * @param circlePreventionSet {@link Set} to prevent infinite loops
	 * @return JSON-LD-structured representation, or null if the result is an empty map
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public Object createMap(Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		if (circlePreventionSet.contains(this)) {
			//TODO: Message: Error in conversion to JSON: cyclic references (Possibly instead change to reference elements by their id)
			//(Propably throw an exception or add handler-reference to containing check)
		}
		circlePreventionSet.add(this);
		Map<String,Object> newMap = new HashMap<>();
		
		fillMapSimple(newMap, circlePreventionSet);
		Object possiblyNewObject = fillMapIndividual(newMap, circlePreventionSet);
		circlePreventionSet.remove(this);
		return possiblyNewObject != null ? possiblyNewObject : !(newMap.isEmpty()) ? newMap : null;//Determine cases where an Object is not displayed as map in the fillMapIndividual-method
	}
	
	
	/**
	 * Fills a map using attributes of this object with a valid ODRL-JSON-LD-representation.
	 * 
	 * @param map {@link Map} to be filled
	 * @param circlePreventionSet {@link Set} to prevent infinite loops
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public void fillMapSimple(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		Class<?> klass = this.getClass();
		while (ODRLClass.class.isAssignableFrom(klass)) {
			for (Field f : klass.getDeclaredFields()) {
				Object termKey = handler.getTermMap().get(f);
				if (termKey instanceof String termKeyString) {
					try {//TODO decide how to handle the exceptions					
						Object termValue = klass.getMethod("get" + f.getName().substring(0, 1).toUpperCase() + f.getName().substring(1)).invoke(this);
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
	
	/**
	 * Completes the JSON-LD-representation of this object based on an existing map-representation.
	 * @param map {@link Map} to be filled
	 * @param circlePreventionSet {@link Set} to prevent infinite loops
	 * @return the object to replace the map with, or null if the map should not be replaced
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		return null;
	}

	/**
	 * Gets the term used in JSON-LD for this Object.
	 * @return the term used
	 */
	public String getClassTerm() {
		return handler.getTermMap().get(this.getClass());
	}
	public String getTypeKeyword() {
		return "@type";
	}
	public String getIdKeyword() {
		return "@id";
	}
	
	/**
	 * Returns the ODRL-term that represents a field of this object's class.
	 * @param fieldName name of the field
	 * @return the ODRL-term
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public String getFieldTerm(String fieldName) throws NoSuchFieldException, SecurityException {
		return getFieldTerm(fieldName,this.getClass());
	}
	
	
	/**
	 * Returns the ODRL-term that represents a field of a class.
	 * @param fieldName name of the field
	 * @param klass the class
	 * @return the ODRL-term
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	public String getFieldTerm(String fieldName, Class<?> klass) throws NoSuchFieldException, SecurityException {
		if (!(ODRLClass.class.isAssignableFrom(klass))) {
			throw new NoSuchFieldException(fieldName);
		}
		try {
			return (handler.getTermMap().get(klass.getDeclaredField(fieldName)));
		} catch (NoSuchFieldException e) {
			Class<?> superKlass = klass.getSuperclass();
			if (ODRLClass.class.isAssignableFrom(superKlass)) {
				return getFieldTerm(fieldName, superKlass);
			} else {
				throw e;
			}
		}
	}
}
