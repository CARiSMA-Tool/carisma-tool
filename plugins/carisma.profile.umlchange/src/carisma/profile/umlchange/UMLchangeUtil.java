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
package carisma.profile.umlchange;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Namespace;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;


/**
 * The implementation of the UMLchange profile.
 * Provides easy access to the profile's stereotypes
 * and to applications of those.
 * @author Daniel Warzecha
 *
 */
public final class UMLchangeUtil {
    
    /**
     * Hide constructor.
     */
    private UMLchangeUtil() {
        
    }

	/**
	 * Returns all elements in the package that UMLchange stereotypes are
	 * applied to. Convenience method for specific UMLchange stereotype query.
	 * @param pkg - the package to search
	 * @return - list of UMLchange stereotyped elements
	 */
	public static List<Element> getStereotypedElements(final Package pkg) {
		return getStereotypedElements(null, pkg);
	}
	
	/**
	 * Returns a list of the subset of elements in the package pkg
	 * to which the given stereotype has been applied. 
	 * If the package itself has the stereotype applied to it, pkg is
	 * also in the list.
	 * If the given stereotype is not in the UMLchange profile, this
	 * function returns null. Otherwise at least an empty list is returned. 
	 * @param stereo - the stereotype to search applications of 
	 * @param pkg - the package to search for applications of the stereotype
	 * @return - a list of elements with applied UMLchange stereotypes.
	 */
	public static List<Element> getStereotypedElements(final UMLchange stereo, final Package pkg) {
		List<Element> extendedElements = new ArrayList<>();		
		if (UMLHelper.isProfileApplied(pkg, UMLchange.DESCRIPTOR)) {
			if (hasStereotype(stereo, pkg)) {
				extendedElements.add(pkg);
			}
			extendedElements.addAll(getStereotypedElements(stereo, pkg.allOwnedElements()));
		}
		return extendedElements;
	}

	public static List<Element> getStereotypedElements(final UMLchange stereo, final Namespace ns) {
		List<Element> extendedElements = new ArrayList<>();		
		if (UMLHelper.isProfileApplied(ns.getModel(), UMLchange.DESCRIPTOR)) {
			if (hasStereotype(stereo, ns)) {
				extendedElements.add(ns);
			}
			extendedElements.addAll(getStereotypedElements(stereo, ns.allOwnedElements()));
		}
		return extendedElements;
	}
	
	public static List<Element> getStereotypedElements(final UMLchange stereo, final NamedElement ne) {
		List<Element> extendedElements = new ArrayList<>();		
		if (UMLHelper.isProfileApplied(ne.getModel(), UMLchange.DESCRIPTOR)) {
			if (hasStereotype(stereo, ne)) {
				extendedElements.add(ne);
			}
			extendedElements.addAll(getStereotypedElements(stereo, ne.allOwnedElements()));
		}
		return extendedElements;
	}
	
	/**
	 * Searches the list of given elements for UMLchange applications and returns
	 * the set of stereotyped elements.
	 * @param stereo - the UMLchange stereotype to search for
	 * @param elements - list of elements to inspect
	 * @return - list of elements with UMLchange applications of stereotype
	 */
	public static List<Element> getStereotypedElements(final UMLchange stereo, final List<Element> elements) {		List<Element> extendedElements = new ArrayList<>();
		for (Element element : elements) {
			if (hasStereotype(stereo, element)) {
				extendedElements.add(element);
			}
		}
		return extendedElements;
	}
	/**
	 * Returns the UMLchange Stereotype elements which are applied to the given element.
	 * @param element - the element to inspect
	 * @return - list of UMLchange stereotypes
	 */
	public static List<Stereotype> getAppliedStereotypes(final Element element) {
		List<Stereotype> result = new ArrayList<>();
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			if (UMLchange.contains(stereo)) {
				result.add(stereo);
			}
		}
		return result;
	}

	/**
	 * Returns the StereotypeApplication of the given stereotype if it is applied.
	 * @param stereo - stereotype
	 * @param element - element
	 * @return stereotypeApplication if found, null otherwise
	 */
	public static StereotypeApplication getStereotypeApplication(
			final UMLchange stereo, final Element element) {
		for (StereotypeApplication stereoApp : getStereotypeApplications(element)) {
			UMLchange type = UMLchange.getValue(stereoApp.getAppliedStereotype().getName());
			if (type.equals(stereo)) {
				return stereoApp;
			}
		}
		return null;
	}

	/**
	 * Returns the UMLchange stereotype applications to the given element.
	 * @param element - the element to inspect
	 * @return - list of UMLchange stereotype applications
	 */
	public static List<StereotypeApplication> getStereotypeApplications(final Element element) {
		List<StereotypeApplication> result = new ArrayList<>();
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			if (UMLchange.contains(stereo)) {
				result.add(new StereotypeApplication(stereo, element));
			}
		}
		return result;
	}
	
	/**
	 * Returns the UMLchange stereotype applications present in the given package.
	 * @param pkg - the package containing UMlchange applications
	 * @return - list of UMLchange applications in package
	 */
	public static List<StereotypeApplication> getStereotypeApplications(final Package pkg) {
		List<StereotypeApplication> applications = new ArrayList<>();
		for (Element extendedElement : getStereotypedElements(pkg)) {
			applications.addAll(getStereotypeApplications(extendedElement));
		}
		return applications;
	}
	
	/**
	 * Returns the UMLchange Stereotype element if the UMLchange stereotype
	 * is applied to the element. 
	 * @param stereo - UMLchange stereotype to find
	 * @param element - element to inspect
	 * @return - UMLchange Stereotype element 
	 */
	public static Stereotype getAppliedStereotype(final UMLchange stereo, final Element element) {
		for (Stereotype appliedStereo : element.getAppliedStereotypes()) {
			if (stereo.isEqual(appliedStereo)) {
				return appliedStereo;
			}
		}
		return null;
	}
	
	/**
	 * Checks if the UMLchange profile is applied to the given package.
	 * @param pkg - package to inspect
	 * @return - true if UMLchange is applied
	 */
	public static boolean isProfileApplied(final Package pkg) {
		if (pkg == null) {
			return false;
		}
		for (Profile appliedProfile : pkg.getAllAppliedProfiles()) {
			if (appliedProfile.getDefinition() != null 
					&& appliedProfile.getDefinition().getNsURI().contains(UMLchange.DESCRIPTOR.getProfileName())) {
				return true;
			}
		}
		return false;
	}
	/**
	 * Checks if the element has an UMLchange stereotype applied to it.
	 * @param elem - element to inspect
	 * @return - true if UMLchange is applied
	 */
	public static boolean hasStereotype(final Element elem) {
		return hasStereotype(null, elem);
	}
	/**
	 * Checks if the element has the specified UMLchange stereotyped applied to it.
	 * @param stereo - UMLchange stereotype to check for
	 * @param elem - element to inspect
	 * @return - true if the specified UMLchange stereotype is applied
	 */
	public static boolean hasStereotype(final UMLchange stereo, final Element elem) {
		for (Stereotype appliedStereo : elem.getAppliedStereotypes()) {
			if (stereo == null) {
				if (UMLchange.contains(appliedStereo)) {
					return true;
				}
			} else if (stereo.isEqual(appliedStereo)) {
				return true;
			}
		}
		return false;
	}

	public static boolean hasPattern(final UMLchange type) {
		if (type == UMLchange.DELALL || type == UMLchange.ADDALL || type == UMLchange.SUBSTALL) {
			return true;
		}
		return false;
	}

	public static boolean hasNew(final UMLchange type) {
		if (type == UMLchange.ADD || type == UMLchange.SUBST) {
			return true;
		}
		if (type == UMLchange.ADDALL || type == UMLchange.SUBSTALL) {
			return true;
		}
		return false;
	}

	public static boolean hasTo(final UMLchange type) {
		if (type == UMLchange.MOVE || type == UMLchange.COPY) {
			return true;
		}
		return false;
	}

}
