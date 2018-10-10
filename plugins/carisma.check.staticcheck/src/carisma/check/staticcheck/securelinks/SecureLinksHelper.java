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
package carisma.check.staticcheck.securelinks;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.UMLDeploymentHelper;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


public final class SecureLinksHelper {
	private SecureLinksHelper() {
		
	}
	
	public static Set<Dependency> getAllRelevantDependencies(final Package pkg) {
		Set<Dependency> relevantDependencies = new HashSet<>();
		if (UMLsecUtil.hasStereotype(pkg, UMLsec.SECURE_LINKS)) {
			relevantDependencies.addAll(UMLHelper.getAllElementsOfType(pkg, Dependency.class));
		} else {
			for (Package containedPkg : UMLHelper.getAllElementsOfType(pkg, Package.class)) {
				if (UMLsecUtil.hasStereotype(containedPkg, UMLsec.SECURE_LINKS)) {
					relevantDependencies.addAll(UMLHelper.getAllElementsOfType(containedPkg, Dependency.class));
				}
			}
		}
		return relevantDependencies;
	}
	
	public static Set<Dependency> getAllDependenciesWithRequirements(final Collection<Artifact> artifacts) {
		Set<Dependency> requirementDependencies = new HashSet<>();
		for (Artifact arti : artifacts) {
			requirementDependencies.addAll(getAllDependenciesWithRequirements(arti));
		}
		return requirementDependencies;
	}
	
	public static Set<Dependency> getAllDependenciesWithRequirements(final Artifact artifact) {
		Set<Dependency> requirementDependencies = new HashSet<>();
		for (Dependency dep : UMLDeploymentHelper.getAllDependencies(artifact)) {
			if (hasSecureLinksRequirements(dep)) {
				requirementDependencies.add(dep);
			}
		}
		return requirementDependencies;
	}
	
	public static boolean inSecureLinksScope(final Element element) {
		Element owner = element.getOwner();
		while (owner != null) {
			if (owner instanceof Package && UMLsecUtil.hasStereotype(owner, UMLsec.SECURE_LINKS)) {
				return true;
			}
			owner = owner.getOwner();
		}
		return false;
	}
	
	public static Set<Stereotype> getAllRequirements(final Dependency dep) {
		Set<Stereotype> allRequirements = new HashSet<>();
		for (Stereotype stereo : dep.getAppliedStereotypes()) {
			if (isSecureLinksRequirement(stereo)) {
				allRequirements.add(stereo);
			}
		}
		return allRequirements;
	}
	
	/**
	 * Checks if the stereotype is a secure links
	 * requirement used on dependencies. 
	 * @param stereotype - the stereotype to check
	 * @return - true if stereotype is requirement
	 */
	public static boolean 
	isSecureLinksRequirement(final Stereotype stereotype) {
		if (!stereotype.getProfile().getName().contains("UMLsec")) {
			return false;
		}
		return isSecureLinksRequirement(stereotype.getName());
	}
	
	/**
	 * Checks if the UMLsec profile contains a stereotype of the given name.
	 * @param stName - name of stereotype to look for
	 * @return - true if UMLsec contains stereotype
	 */
	public static boolean isSecureLinksRequirement(final String stName) {
		List<String> nameParts = Arrays.asList(stName.split("::"));
		String stereoName = nameParts.get(nameParts.size() - 1);
		if (stereoName.equalsIgnoreCase("secrecy") 
				|| stereoName.equalsIgnoreCase("integrity") 
				|| stereoName.equalsIgnoreCase("high")) {
			return true;
		}				
		return false;
	}
	
	/**
	 * Checks if the dependency has Secure Links
	 * requirements at all.
	 * @param aDep - the dependency to check
	 * @return - true if it has any
	 */
	public static boolean hasSecureLinksRequirements(final Dependency aDep) {
		for (Stereotype stereo : aDep.getAppliedStereotypes()) {
			if (isSecureLinksRequirement(stereo)) {
				return true;
			}
		}
		return false;
	}
	/**
	 * Checks if the stereotype is a secure links
	 * linktype used on links.
	 * @param stereotype - the stereotype to check
	 * @return - true if stereotype is linktype
	 */
	public static boolean 
	isSecureLinksLinktype(final Stereotype stereotype) {
		String stName = stereotype.getName().toLowerCase(Locale.ENGLISH);
		if (!stereotype.getProfile().getName().contains("UMLsec")) {
			return false;
		}
		return isSecureLinksLinktype(stName);
	}
	
	public static boolean 
	isSecureLinksLinktype(final String stName) {
		List<String> nameParts = Arrays.asList(stName.split("::"));
		String stereoName = nameParts.get(nameParts.size() - 1);
		if (stereoName.equalsIgnoreCase("internet") 
				|| stereoName.equalsIgnoreCase("lan") 
				|| stereoName.equalsIgnoreCase("wire") 
				|| stereoName.equalsIgnoreCase("encrypted")) {
			return true;
		}
		return false;
	}
	/**
	 * Checks if the dependency has Secure Links
	 * requirements at all.
	 * @param commPath - the communication path to check
	 * @return - true if it has a linktype
	 */
	
	public static boolean hasSecureLinksLinktype(final CommunicationPath commPath) {
		for (Stereotype stereo : commPath.getAppliedStereotypes()) {
			if (isSecureLinksLinktype(stereo)) {
				return true;
			}
		}
		return false;
	}	

}
