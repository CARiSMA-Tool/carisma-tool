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
package carisma.profile.umlsec.umlsec4ids;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;


/**
 * The implementation of the UMLsec profile.
 * Provides easy access to the profile's stereotypes
 * and to applications of those.
 * @author Daniel Warzecha
 *
 */
public final class UMLsecUtil {
    
    /** Hide constructor.
     */
    private UMLsecUtil() {
    }
	
	/**
	 * Returns a list of the subset of elements in the package pkg
	 * to which a UMLsec stereotype has been applied.
	 * @param pkg
	 * @return
	 */
	public static List<Element> getStereotypedElements(final Package pkg) {
		return getStereotypedElements(pkg, null);
	}
	
	/**
	 * Returns a list of the subset of elements in the package pkg
	 * to which the given stereotype has been applied. 
	 * If the package itself has the stereotype applied to it, pkg is
	 * also in the list.
	 * If the given stereotype is not in the UMLsec profile, this
	 * function returns null. Otherwise at least an empty list is returned. 
	 * @param stereo - the stereotype to search applications of 
	 * @param pkg - the package to search for applications of the stereotype
	 * @return - a list of elements with applied UMLsec stereotypes.
	 */
	public static List<Element> getStereotypedElements(final Package pkg, final UMLsec stereo) {
		List<Element> extendedElements = new ArrayList<Element>();		
		if (UMLHelper.isProfileApplied(pkg, UMLsec.DESCRIPTOR)) {
			if (hasStereotype(pkg, stereo)) {
				extendedElements.add(pkg);
			}
			extendedElements.addAll(getStereotypedElements(pkg.allOwnedElements(), stereo));
		}
		return extendedElements;
	}
	
	public static List<Element> getStereotypedElements(final List<Element> elements, final UMLsec stereo) {
		List<Element> extendedElements = new ArrayList<Element>();		
		for (Element element : elements) {
			if (hasStereotype(element, stereo)) {
				extendedElements.add(element);
			}
		}
		return extendedElements;
	}
	
	/**
	 * Returns the StereotypeApplication of the given stereotype if it is applied.
	 * @param stereo - stereotype
	 * @param element - element
	 * @return stereotypeApplication if found, null otherwise
	 */
	public static StereotypeApplication getStereotypeApplication(
			final Element element, final UMLsec stereo) {
		for (StereotypeApplication stereoApp : getStereotypeApplications(element, stereo)) {
			UMLsec type = UMLsec.getValue(stereoApp.getAppliedStereotype().getName());
			if (type.equals(stereo)) {
				return stereoApp;
			}
		}
		return null;
	}
	
	/**
	 * Returns the UMLsec stereotype applications present in the given package.
	 * @param pkg - the package containing UMLsec applications
	 * @return - list of UMLsec applications in package
	 */
	public static List<StereotypeApplication> getStereotypeApplications(final Package pkg) {
		return getStereotypeApplications(pkg, null);
	}
	
	/**
	 * Returns the UMLsec stereotype applications of the given UMLsec stereotype present in the given package.
	 * @param pkg - the package containing UMLsec applications
	 * @return - list of UMLsec applications in package of given type
	 */
	public static List<StereotypeApplication> getStereotypeApplications(final Package pkg, final UMLsec stereotype) {
		List<StereotypeApplication> applications = new ArrayList<StereotypeApplication>();
		for (Element extendedElement : getStereotypedElements(pkg, stereotype)) {
			applications.add(getStereotypeApplication(extendedElement, stereotype));
		}
		return applications;
	}
	

	/**
	 * Returns the UMLsec stereotype applications to the given element.
	 * @param element - the element to inspect
	 * @return - list of UMLsec stereotype applications
	 */
	public static List<StereotypeApplication> getStereotypeApplications(final Element element, final UMLsec stereotype) {
		List<StereotypeApplication> result = new ArrayList<StereotypeApplication>();
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			if (UMLsec.contains(stereo)) {
				result.add(new StereotypeApplication(stereo, element));
			}
		}
		return result;
	}
	
	public static boolean hasStereotype(final Element elem) {
		return hasStereotype(elem, null);
	}
	
	public static boolean hasStereotype(final Element elem, final UMLsec stereo) {
		for (Stereotype appliedStereo : elem.getAppliedStereotypes()) {
			if (stereo == null) {
				if (UMLsec.contains(appliedStereo)) {
					return true;
				}
			} else if (stereo.isEqual(appliedStereo)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Takes the list of tag values of the given UMLsec stereotype at the element stereoParent
	 * and filters and converts the list elements of the type string.  
	 * @param tagName - The name of the tag to read the values of
	 * @param stereo - the stereotype with the tag
	 * @param stereoParent - the element the stereotype is applied to
	 * @return - a list of string tag values; empty if the stereotype doesn't have the tag
	 */
	public static List<String> getStringValues(final String tagName, final UMLsec stereo, final Element stereoParent) {
		List<String> tagValues = new ArrayList<String>();
		List<Object> tmp = getTaggedValues(tagName, stereo, stereoParent);
		for (Object obj : tmp) {
			if (obj instanceof String) {
				tagValues.add((String) obj);
			}
		}
		return tagValues;
	}
	/**
	 * Returns a list of the tag values of the UMLsec stereotype at the given element stereoParent.
	 * @param tagName - The name of the tag to read the values of
	 * @param stereo - the stereotype with the tag
	 * @param stereoParent - the element the stereotype is applied to
	 * @return - a list of tag values; empty if the stereotype doesn't have the tag
	 */
	@SuppressWarnings("unchecked")
	public static List<Object> getTaggedValues(final String tagName, final UMLsec stereo, final Element stereoParent) {
		List<Object> tagValues = new ArrayList<Object>();
		if (stereoParent == null) {
			return tagValues;
		}
		StereotypeApplication stereoApp = getStereotypeApplication(stereoParent, stereo);
		if (stereoApp == null) {
			return tagValues;
		}
		TaggedValue tv = stereoApp.getTaggedValue(tagName);
		if (tv == null) {
			return tagValues;
		}
		Object tagValueObject = tv.getValue();
		if (tagValueObject instanceof EList<?>) {
			EList<Object> valueList = (EList<Object>) tagValueObject;
			tagValues.addAll(valueList);
		} else {
			tagValues.add(tagValueObject);
		}
		return tagValues;
	}

	public static boolean isInScopeOfStereotype(final Element element, final UMLsec stereotype) {
		if (hasStereotype(element, stereotype)) {
			return true;
		}
		Element owner = element.getOwner();
		while (owner != null) {
			if (hasStereotype(owner, stereotype)) {
				return true;
			}
			owner = owner.getOwner();
		}
		return false;
	}
}