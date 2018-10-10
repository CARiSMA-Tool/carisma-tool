/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.modeltype.uml2;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.common.util.WrappedException;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.impl.BasicEObjectImpl;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.PrimitiveType;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Relationship;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.Transition;
import org.eclipse.uml2.uml.UMLPackage;
import org.eclipse.uml2.uml.Vertex;
import org.eclipse.uml2.uml.resource.UMLResource;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;


/**
 * This class contains convenience methods to easier access UML2 models.
 * 
 * @author wenzel
 * @author Daniel Warzecha
 *
 */
public final class UMLHelper {
	
	/** 
	 * Hiding constructor.
	 */
	private UMLHelper() {
	}
	
	/**
	 * Constant String for output.
	 */
	private static final String APOSTROPHE = "'";

    /**
     * Constant String for output.
     */
	private static final String OPENING = "' of <<";

    /**
     * Constant String for output.
     */
	private static final String CLOSING = ">> is not of type '";
	
	/**
	 * Returns the metaclass object specified by the given name. e.g. "Class" or "Association"
	 * @param name
	 * @return
	 */
	public static EClass getMetaClass(String name) throws InvalidMetaclassException {
		EPackage ePackage = UMLPackage.eINSTANCE;
		EClassifier eClassifier = ePackage.getEClassifier(name);
		if (eClassifier instanceof EClass) {
			return (EClass) eClassifier;
		}
		throw new InvalidMetaclassException(name + " is not a valid UML2 metaclass");
	}
	
// TODO: Use enumeration for PrimitiveTypes
	public static PrimitiveType getPrimitiveType(final Package model, final String name) {
		if (!primitiveTypesImported(model)) {
			importPrimitiveTypePackage(model);
		}
		// FIXME: Nochmal drueberschauen und aufraeumen
		for (Package pkg : model.getImportedPackages()) {
			if (pkg.getName().equalsIgnoreCase("UMLPrimitiveTypes")) {
				return (PrimitiveType) pkg.getMember(name);
			}
		}
		return null;
	}
	
	private static boolean primitiveTypesImported(final Package model) {
		if (model.getImportedPackages().isEmpty()) {
			return false;
		}
		for (Package pkg : model.getImportedPackages()) {
			if (pkg.getName().equalsIgnoreCase("UMLPrimitiveTypes")) {
				return true;
			}
		}
		return false;
	}
	
	private static void importPrimitiveTypePackage(final Package model) {
		Model umllibrary = (Model) loadPackage(
				model.eResource().getResourceSet(),
				URI.createURI(UMLResource.UML_PRIMITIVE_TYPES_LIBRARY_URI));
		model.createPackageImport(umllibrary);
	}
	
	protected static org.eclipse.uml2.uml.Package loadPackage(final ResourceSet intoSet, URI uri) {
        org.eclipse.uml2.uml.Package loadedPackage = null;

        try {
             Resource resource = intoSet.getResource(uri, true);

             loadedPackage = (org.eclipse.uml2.uml.Package) EcoreUtil.getObjectByType(
                       resource.getContents(), UMLPackage.Literals.PACKAGE);
        } catch (WrappedException we) {
        	Logger.log(LogLevel.ERROR, we.getMessage(), we);
        	return null;
        }

     return loadedPackage;
	}
	
	/**
	 * Checks if the given value is an UMLPrimitivetype in the given model.
	 * @param model - the model where to check the imported primitive types 
	 * @param value - the value to check if it is a primitive type
	 * @return - true if the value is a UMLPrimitiveType in the model
	 */
	public static boolean isPrimitiveType(final Model model, final String value) {
		if (UMLHelper.getPrimitiveType(model, value) != null) {
			return true;
		}
		return false;
	}
	
	/**
	 * Returns all model elements of the given element which are of the given type. 
	 * @param <T>
	 * @param model
	 * @param type
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T> List<T> getAllElementsOfType(Element inThisElement, Class<T> type) {
		List<T> result = new ArrayList<>();
		TreeIterator<EObject> iterator = inThisElement.eAllContents();
		while (iterator.hasNext()) {
			EObject element = iterator.next();
			if (type.isInstance(element)) {
				result.add((T) element);
			}
		}
		return result;
	}
	
	public static List<Element> getAllElementsOfType(Element inThisElement, EClass mc) {
		List<Element> result = new ArrayList<>();
		TreeIterator<EObject> iterator = inThisElement.eAllContents();
		while (iterator.hasNext()) {
			EObject element = iterator.next();
			if (element instanceof Element && element.eClass().equals(mc)) {
				result.add((Element) element);
			}
		}
		return result;
	}
	
	public static List<NamedElement> getAllSameNameElements(final Package pkg, final String unqualifiedName) {
		List<NamedElement> sameNameElements = new ArrayList<>();
		for (Element elem : pkg.allOwnedElements()) {
			if (elem instanceof NamedElement) {
				NamedElement ne = (NamedElement) elem;
				if (ne.getName().equalsIgnoreCase(unqualifiedName)) {
					sameNameElements.add(ne);
				}
			}
		}
		return sameNameElements;
	}
	
	
	public static List<Element> getAllElements(final Package pkg){
		List<Element> allElements = new ArrayList<>();
		for(Element elem: pkg.allOwnedElements()){
			allElements.add(elem);
		}
		
		return allElements;
	}
	
	/**
	 * Among all elements of the given type, finds model elements that match the given hopefully adequately qualified name.
	 * Adequately means that in a model where two elements have the same name, but are in different namespaces, the wanted element
	 * is qualified so far that it is unique in the model scope.
	 * Example: A model, two packages, each has a class named C. Qualified name for both is model::Pkg1::C and model::Pkg2::C respectively.
	 * One has to reference a class C with Pkg1::C or Pkg2::C. The model prefix is not needed.
	 * @param pkg - the package to search
	 * @param adequatelyQualifiedName - the name of the element to look for
	 * @param type - the class of the element to look for
	 * @param <T> generic return Type.
	 * @throws ModelElementNotFoundException if there is no such Element or it is not unique.
	 * @return - the element
	 */
	public static <T extends NamedElement> T getElementOfNameAndType(final Package pkg, final String adequatelyQualifiedName, final Class<T> type)
	throws ModelElementNotFoundException {
		List<T> matchingElements = new ArrayList<>();
		for (T namedElem : getAllElementsOfType(pkg, type)) {
			String name = namedElem.getName();
			if ((name != null && !name.isEmpty()) 
					&& (name.endsWith(adequatelyQualifiedName) 
					&& !matchingElements.contains(namedElem))) {
				Pattern qualifiedPattern = Pattern.compile("(?<=(^|::))" + adequatelyQualifiedName);
				Matcher qualifiedMatcher = qualifiedPattern.matcher(name);
				if (qualifiedMatcher.find()) {
					matchingElements.add(namedElem);
				}
			}
		}
		if (matchingElements.isEmpty()) {
			throw new ModelElementNotFoundException(
					"Couldn't find element " + adequatelyQualifiedName + " of type " + type.getName() + " in package " + pkg.getName() + ".");
		} else if (matchingElements.size() == 1) {
			return matchingElements.get(0);
		} else {

			StringBuffer exceptionMessage = new StringBuffer(
					type.getSimpleName() + " " + adequatelyQualifiedName + " not adequately qualified! Found " + matchingElements.size() + " Elements:");
			for (T element : matchingElements) {
				exceptionMessage.append(System.getProperty("line.separator"));
				exceptionMessage.append("	found ");
				exceptionMessage.append(element.getQualifiedName());
			}
			throw new ModelElementNotFoundException(exceptionMessage.toString());
//			Logger.log(LogLevel.WARNING, type.getSimpleName() + " " + adequatelyQualifiedName + " not adequately qualified! Unpredictable behaviour possible!");
//			return matchingElements.get(0);
		}
	}
	
	/**
	 * Returns the named element from the model if present.
	 * Warns if multiple elements in the model have the same name and are in the same scope part. See getElementByNameAndType. 
	 * @param model - model to search
	 * @param adequatelyQualifiedName - qualified(contains ::) or unqualified name of the element to look for
	 * @return the NamedElement object 
	 */
	public static NamedElement getElementByName(final Model model, final String adequatelyQualifiedName) throws ModelElementNotFoundException {
		return getElementOfNameAndType(model, adequatelyQualifiedName, NamedElement.class);
	}
		
	/**
	 * If the given element is extended with the stereotype of the given name,
	 * the corresponding Stereotype is returned. If not, null is returned. 
	 * @param element - element to check for a stereotype
	 * @param unqualifiedName - stereotype name 
	 * @return - the applied stereotype or null
	 */
	public static Stereotype getAppliedStereotype(final Element element, final String unqualifiedName) {
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			if (stereo.getName().equalsIgnoreCase(unqualifiedName)) {
				return stereo;
			}
		}
		return null;
	}
	
	@SuppressWarnings("unchecked")
	public static <T> T getSingleTaggedValue(final Element element, final String unqualifiedStereotypeName, final String tagName, Class<T> type) {
		T result = null;
		Stereotype st = getAppliedStereotype(element, unqualifiedStereotypeName);
		if (st != null) {
			Object firstResult = element.getValue(st, tagName);
			if (firstResult instanceof Collection<?>) {
				Collection<?> firstCol = ((Collection<?>) firstResult);
				if (!firstCol.isEmpty()) {
					Object firstElement = firstCol.iterator().next();
					if (type.isInstance(firstElement)) {
						result = (T) firstElement;
					} else {
						throw new IllegalArgumentException(
								"Tag '" + tagName + OPENING + unqualifiedStereotypeName + CLOSING + type.getName() + APOSTROPHE);
					}
				}
			} else {
				if (type.isInstance(firstResult)) {
					result = (T) firstResult;
				} else {
					throw new IllegalArgumentException(
							"Tag '" + tagName + OPENING + unqualifiedStereotypeName + CLOSING + type.getName() + APOSTROPHE);
				}
			}
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	public static <T> List<T> getManyTaggedValue(final Element element, final String unqualifiedStereotypeName, final String tagName, Class<T> type) {
		List<T> result = null;
		Stereotype st = getAppliedStereotype(element, unqualifiedStereotypeName);
		if (st != null) {
			Object firstResult = element.getValue(st, tagName);
			if (firstResult instanceof Collection<?>) {
				Collection<?> firstCollection = ((Collection<?>) firstResult);
				if (firstCollection.isEmpty()) {
					result = new ArrayList<>();
				} else {
					Object firstElement = firstCollection.iterator().next();
					if (type.isInstance(firstElement)) {
						Collection<T> firstCollectionT = (Collection<T>) firstCollection;
						result = new ArrayList<>(firstCollectionT);
					} else {
						throw new IllegalArgumentException(
								"Tag '" + tagName + OPENING + unqualifiedStereotypeName + CLOSING + type.getName() + APOSTROPHE);
					}
				}
			} else {
				if (type.isInstance(firstResult)) {
					result = Collections.singletonList((T) firstResult);
				} else {
					throw new IllegalArgumentException(
							"Tag '" + tagName + OPENING + unqualifiedStereotypeName + CLOSING + type.getName() + APOSTROPHE);
				}
			}
		}
		return result;
	}
	
	/**
	 * If the given element is extended with the stereotype of the given name,
	 * the StereotypeApplication is returned. If not, null is returned. 
	 * @param element - element to check for a stereotype
	 * @param unqualifiedName - stereotype name 
	 * @return - the StereotypeApplication or null
	 */	
	public static StereotypeApplication getStereotypeApplication(final Element element, final String unqualifiedName) {
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			if (stereo.getName().equalsIgnoreCase(unqualifiedName)) {
				return new StereotypeApplication(stereo, element);
			}
		}
		return null;
	}
	
	public static List<StereotypeApplication> getStereotypeApplications(final Element element) {
		List<StereotypeApplication> applications = new ArrayList<>();
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			applications.add(new StereotypeApplication(stereo, element));
		}
		return applications;
	}
	
	public static Property getTag(final Stereotype stereo, final String tagName) {
		if (hasTag(stereo, tagName)) {
			return stereo.getAttribute(tagName, null);
		}
		return null;
	}
	
	public static boolean hasTag(final Stereotype stereo, final String tagName) {
		if (stereo.getAttribute(tagName, null) != null) {
			return true;
		}
		return false;
	}
	
	/**
	 * Checks if the given element is extended with the stereotype of the given name.
	 * @param element - element to check for a stereotype
	 * @param unqualifiedName - stereotype name 
	 * @return - true if the stereotype is applied
	 */
	public static boolean isStereotypeApplied(final Element element, final String unqualifiedName) {
		return getAppliedStereotype(element, unqualifiedName) != null;
	}
	
	
	/**
	 * Searches the given package for extensions and stores each stereotype
	 * and its extended elements in a map.
	 * @param pkg - package to search
	 * @return - map of stereotype applications
	 */
	public static Map<Stereotype, List<Element>> getExtendedElements(final Package pkg) {
		HashMap<Stereotype, List<Element>> extendedElemsMap = new HashMap<>();
		for (Element elem : pkg.allOwnedElements()) {
			for (Stereotype appliedStereo : elem.getAppliedStereotypes()) {
				if (extendedElemsMap.containsKey(appliedStereo)) {
					extendedElemsMap.get(appliedStereo).add(elem);
				} else {
					List<Element> extendedElems = new ArrayList<>();
					extendedElems.add(elem);
					extendedElemsMap.put(appliedStereo, extendedElems);
				}
			}
		}
		return extendedElemsMap;
	}
	
	/**
	 * Searches the given package for applications of the given stereotype
	 * and returns the list of extended elements.
	 * @param pkg - the package to search
	 * @param stereo - the stereotype to look for
	 * @return - list of extended elements
	 */
	public static List<Element> getExtendedElements(final Package pkg, final Stereotype stereo) {
		List<Element> extendedElems = new ArrayList<>();
		for (Element elem : pkg.allOwnedElements()) {
			if (elem.isStereotypeApplied(stereo)) {
				extendedElems.add(elem);
			}
		}
		return extendedElems;
	}
	
	public static <T extends Element> List<T> getExtendedElements(final Package pkg, final Stereotype stereo, final Class<T> type) {
		List<T> extendedElems = new ArrayList<>();
		for (Element elem : pkg.allOwnedElements()) {
			if (type.isInstance(elem)) {
				@SuppressWarnings("unchecked")
				T t = (T) elem;
				if (t.isStereotypeApplied(stereo)) {
					extendedElems.add(t);
				}
			}
		}
		return extendedElems;
	}

	/**
	 * Returns the model the element belongs to.
	 * @param obj - the element to check
	 * @return - the model of the element
	 */
	public static Model getModel(final EObject obj) {
		if (obj instanceof Element) {
			Element elem = (Element) obj;
			return elem.getModel();
		}
		if (obj instanceof StereotypeApplication) {
			StereotypeApplication stapp = (StereotypeApplication) obj;
			return stapp.getExtendedElement().getModel();
		}
		if (obj instanceof TaggedValue) {
			TaggedValue tagValue = (TaggedValue) obj;
			return tagValue.getCorrespondingApplication().getExtendedElement().getModel();
		}
		return null;
	}
	
	/**
	 * Applies the given qualified stereotype to the given element.
	 * @param element - the element to apply the stereotype to
	 * @param qualifiedStereoName - the name of the stereotype to apply
	 * @return - the stereotype application or null if the stereotype could not be applied
	 */
	public static StereotypeApplication applyStereotype(Element element, final String qualifiedStereoName) {
		String[] name = qualifiedStereoName.split("::");
		Profile stereoProfile = element.getModel().getAppliedProfile(name[0]);
		if (stereoProfile != null) {
			Stereotype stereo = stereoProfile.getOwnedStereotype(name[1]);
			if (stereo != null && (element.applyStereotype(stereo) != null))  {
				return new StereotypeApplication(stereo, element);					
			}
		}
		Logger.log(LogLevel.ERROR, "Model doesn't have any profile applied which contains the Stereotype " + qualifiedStereoName + ".");
		return null;
	} 
	
	public static boolean unapplyStereotype(final Element element, final String qualifiedStereoName) {
		String[] name = qualifiedStereoName.split("::");
		Profile stereoProfile = element.getModel().getAppliedProfile(name[0]);
		if (stereoProfile != null) {
			Stereotype stereo = stereoProfile.getOwnedStereotype(name[1]);
			if (stereo != null) {
				element.unapplyStereotype(stereo);
				return true;
			}
		}
		Logger.log(LogLevel.ERROR, "Model doesn't have any profile applied which contains the Stereotype " + qualifiedStereoName + ".");
		return false;
	}
	/**
	 * Applies a profile to a package.
	 * @param pkg
	 * @param profileURI e.g. "pathmap://UMLsec/UMLsec.uml"
	 * @return
	 */
	public static Profile applyProfile(Package pkg, String profileURI) {
		Profile profile = appliedProfiles.get(profileURI);
		if (profile == null) {
			URI profileUri = URI.createURI(profileURI);
			ResourceSet resourceSet = new ResourceSetImpl();
			Resource profileResource = resourceSet.getResource(profileUri, true);
			try {
				profileResource.load(null);
				for (EObject anObject : profileResource.getContents()) {
					if (anObject instanceof Profile) {
						profile = (Profile) anObject;
						break;
					}
				}
			} catch (IOException e) {
				Logger.log(LogLevel.INFO, "Couldn't find profile " + profileURI, e);
			}
			appliedProfiles.put(profileURI, profile);
		}
		if (profile != null) {
			pkg.applyProfile(profile);
			return profile;
		}
		return null;
	}
	
	private static Map<String, Profile> appliedProfiles = new HashMap<>();
	
	public static String createStereotypeString(Element element) {
		StringBuffer string = new StringBuffer();
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			string.append(", <<" + stereo.getName() + ">>");
			for (Property prop : stereo.getAllAttributes()) {
				string.append("{" + prop.getName() + "=" + element.getValue(stereo, prop.getName()) + "}");
			}
		}
		return string.substring(2);
	}
	
	public static String createSimpleStereotypeString(Element element) {
		StringBuffer string = new StringBuffer();
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			string.append(", <<" + stereo.getName() + ">>");
		}
		return string.substring(2);
	}
	
	/**
	 * Returns the name and the stereotypes of an element, e.g. "<<Interface>> Vehicle" 
	 * @param element
	 * @return
	 */
	public static String createStereotypedNameString(Element element) {
		String string = createSimpleStereotypeString(element);
		if (element instanceof NamedElement) {
			string += " " + ((NamedElement) element).getName();
		} else {
			string += " (" + element.eClass().getName() + ")";
		}
		return string;
	}
	
	/**
	 * Returns the name and the stereotypes of an element, e.g. "<<Interface>> de.cars.Vehicle" 
	 * @param element
	 * @return
	 */
	public static String createQualifiedStereotypedNameString(Element element) {
		String string = createSimpleStereotypeString(element);
		if (element instanceof NamedElement) {
			NamedElement e = (NamedElement) element;
			String qn = e.getQualifiedName();
			if (qn == null) {
				qn = e.getName();
			}
			string += " " + qn;
		} else {
			string += " (" + element.eClass().getName() + ")";
		}
		return string;
	}
	
	public static boolean isConnection(final Element element) {
		if (element instanceof Relationship) {
			return true;
		}
		if (element instanceof Transition) {
			return true;
		}
		return false;
	}
	
	public static List<Element> getAllConnections(final Element element) {
		List<Element> connections = new ArrayList<>();
		for (Relationship relation : element.getRelationships()) {
			if (!connections.contains(relation)) {
				connections.add(relation);
			}
		}
		if (element instanceof Vertex) {
			Vertex vert = (Vertex) element;
			for (Transition incomingTransition : vert.getIncomings()) {
				if (!connections.contains(incomingTransition)) {
					connections.add(incomingTransition);
				}
			}
			for (Transition outgoingTransition : vert.getOutgoings()) {
				if (!connections.contains(outgoingTransition)) {
					connections.add(outgoingTransition);
				}
			}			
		}
		return connections;
	}
	
	
	/**
	 * Checks for a given resource if the profile with the given name is applied and returns it.
	 * @param resource Resource in which the profile application is searched
	 * @param profileName The name of the profile to be searched
	 * @return The profile if found
	 * @throws IllegalArgumentException if different profiles with the given name are applied
	 */
	public static Profile getProfileIfApplied(Resource resource, CarismaProfileDescriptor profileDescriptor) {
		Profile profile = null;
		for (EObject contents : resource.getContents()) {
			if (contents instanceof Package) {
				for (Profile appliedProfile : ((Package) contents).getAllAppliedProfiles()) {
					if (appliedProfile.getDefinition() != null 
							&& appliedProfile.getDefinition().getNsURI().contains(profileDescriptor.getProfileName())) {
						if (profile != null && profile != appliedProfile) {
							throw new IllegalArgumentException("Different profile with name '" + profileDescriptor.getProfileName() + "' are applied!");
						}
						profile = appliedProfile;
					}
				}
			}
		}
		return profile;
	}
	
	public static boolean isProfileApplied(Package pkg, CarismaProfileDescriptor profileDescriptor) {
		if (pkg == null) {
			return false;
		}
		for (Profile appliedProfile : pkg.getAllAppliedProfiles()) {
			if(appliedProfile.eIsProxy()){
				URI uri = ((BasicEObjectImpl) appliedProfile).eProxyURI();
				URI trimmed = uri.trimFragment();
				if(trimmed.toString().contains(profileDescriptor.getProfileName())){
					return true;
				}
			}
			if ((appliedProfile.getDefinition() != null)
					&& appliedProfile.getDefinition().getNsURI().contains(profileDescriptor.getProfileName())) {
				return true;
			}
		}
		return false;
	}

	public static Profile getOrApplyProfile(Resource resource, CarismaProfileDescriptor profileDescriptor) {
		Profile profile = getProfileIfApplied(resource, profileDescriptor);
		if (profile == null) {
			profile = getProfileFromResource(profileDescriptor);
			for (EObject contents : resource.getContents()) {
				if (contents instanceof Package) {
					Package pkg = (Package) contents;
					pkg.applyProfile(profile);
				}
			}
		}
		return profile;
	}
	
	public static Profile getProfileFromResource(CarismaProfileDescriptor profileDescriptor) {
		Profile profile = null;
		URI profileUri = URI.createURI(profileDescriptor.getProfileURI());
		ResourceSet resourceSet = new ResourceSetImpl();
		Resource profileResource = resourceSet.getResource(profileUri, true);
		try {
			profileResource.load(null);
			for (EObject anObject : profileResource.getContents()) {
				if (anObject instanceof Profile) {
					profile = (Profile) anObject;
					break;
				}
			}
		} catch (IOException e) {
			throw new RuntimeException("Profile " + profileDescriptor.getProfileName() + " not found in resources!", e);
		}
		return profile;
	}
	
	public static String getName(final EObject obj) {
		if (obj == null) {
			return "";
		}
		if (obj instanceof NamedElement) {
			return ((NamedElement) obj).getName();
		} else if (obj instanceof StereotypeApplication) {
			return ((StereotypeApplication) obj).getName();
		} else if (obj instanceof TaggedValue) {
			return ((TaggedValue) obj).getName();
		} else {
			return "[unnamed EObject]";
		}
	}
	
	public static String getAdequateQualifiedName(Package pkg, NamedElement namedElement) {
		if (pkg != null && namedElement != null) {
			String qualifiedName = namedElement.getQualifiedName();
			if (!qualifiedName.isEmpty()) {
				// Initialize the variables for search
				String adequateQualifiedName = qualifiedName.substring(qualifiedName.lastIndexOf(":")+1);
				String restOfQualifiedName = qualifiedName.substring(0,qualifiedName.lastIndexOf(":")-1);
				String unqualifiedName = adequateQualifiedName;
				int sameQualifiedElement;
				List<NamedElement> sameNamedElements = UMLHelper.getAllSameNameElements(pkg, unqualifiedName);
				// Searching for a adequate qualified name stops when it equals the qualified name
				while (!qualifiedName.equals(adequateQualifiedName)) {
					// Looking for qualified names of elements, which ends with the same substring
					sameQualifiedElement = 0;
					for (NamedElement sameNamedElement : sameNamedElements) {
						if (sameNamedElement.getQualifiedName().endsWith(adequateQualifiedName)) {
							sameQualifiedElement++;
						}
					}
					
					// If there is only the actual element, the adequate qualified name is returned
					// otherwise it has to be concatenated with the next hierarchy string
					// (e.g. <parent element name>:: + <actual string>)
					if (sameQualifiedElement == 1) {
						return adequateQualifiedName;
					}
					adequateQualifiedName = restOfQualifiedName.substring(restOfQualifiedName.lastIndexOf(":")+1) + "::" + adequateQualifiedName;
					if (restOfQualifiedName.lastIndexOf(":") != -1) {
						restOfQualifiedName = restOfQualifiedName.substring(0,restOfQualifiedName.lastIndexOf(":")-1);
					}
				}
				return adequateQualifiedName;
			}
		}
		
		return "";
	}
}
