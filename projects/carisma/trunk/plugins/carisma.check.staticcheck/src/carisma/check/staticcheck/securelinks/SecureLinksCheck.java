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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;

import carisma.check.staticcheck.securelinks.utils.AnalysisMessage;
import carisma.check.staticcheck.securelinks.utils.OutputTarget;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLDeploymentHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;

/**
 * Functions to process UMLsec properties.
 * @author Daniel Warzecha
 *
 */
public final class SecureLinksCheck {
    /**
     * Constant String for action 'read'.
     */
    private static final String READ = "read";
    /**
     * Constant String for action 'insert'.
     */
    private static final String INSERT = "insert";
    /**
     * Constant String for action 'delete'.
     */
    private static final String DELETE = "delete";
    
	private List<AnalysisMessage> errorMessages = null;

	public SecureLinksCheck() {
		errorMessages = new ArrayList<AnalysisMessage>();
	}
	
	public List<AnalysisMessage> getErrorMessages() {
		return Collections.unmodifiableList(errorMessages);
	}
	
	public int checkSecureLinks(final Package pkg) {
		errorMessages.clear();
		for (Dependency dep : SecureLinksHelper.getAllRelevantDependencies(pkg)) {
			if (SecureLinksHelper.hasSecureLinksRequirements(dep)) {
				for (CommunicationPath commPath : UMLDeploymentHelper.getCommunicationPaths(dep)) {
					errorMessages.addAll(compliesWithRequirements(commPath, dep));
				}
				Map<Node, Node> unconnectedNodes = UMLDeploymentHelper.getUnconnectedNodes(dep);
				for (Entry<Node, Node> entry : unconnectedNodes.entrySet()) {
					Node targetNode = entry.getValue();
					errorMessages.add(
							new AnalysisMessage(
									StatusType.ERROR, 
									OutputTarget.BOTH, 
									Messages.nodesNotConnected(entry.getKey(), targetNode, dep)));
				}
			}
		}
		return errorMessages.size();
	}
	
	
	/**
	 * Returns the set of Secure Links requirements
	 * the dependency has.
	 * @param aDep - the dependency to search
	 * @return - the set of requirements
	 */
	public Set<Stereotype> getRequirements(
			final Dependency aDep) {
		HashSet<Stereotype> requirements = new HashSet<Stereotype>();
		for (Stereotype stereo : aDep.getAppliedStereotypes()) {
			if (SecureLinksHelper.isSecureLinksRequirement(stereo)) {
				requirements.add(stereo);
			}
		}
		return requirements;
	}
	
	/**
	 * Returns the Secure Links linktype.
	 * TODO: Only one linktype per link allowed.
	 * @param aLink - the link to search
	 * @return - the linktype
	 */
	public StereotypeApplication getLinktype(final CommunicationPath aLink) {
		if (aLink != null) {
			for (Stereotype stereo : aLink.getAppliedStereotypes()) {
				if (SecureLinksHelper.isSecureLinksLinktype(stereo)) {
					return new StereotypeApplication(stereo, aLink);
				}
			}
		}
		return null;
	}
	
	/**
	 * Checks if the link (or rather the linktype)
	 * complies with the stereotype requirement.
	 * @param aLink - the link in question
	 * @param stRequirement - the requirement stereotype
	 * @return - true if the link complies with the requirement
	 */
	public List<AnalysisMessage> compliesWithRequirement(
		final CommunicationPath aLink, final Stereotype stRequirement) {
		List<AnalysisMessage> errors = new ArrayList<AnalysisMessage>();
		String attacker = getAttacker(aLink);
		Set<String> threats = getThreats(aLink);
		List<String> violations = getViolations(stRequirement, threats);
		if (!violations.isEmpty()) {
			errors.add(
					new AnalysisMessage(
							StatusType.ERROR,
							OutputTarget.BOTH,
							Messages.secureLinksViolated(attacker, aLink, violations)));
		}
		return errors;
	}
	
	/**
	 * Returns the violations of the attacker given the set of threats and
	 * the requirement to fulfill.
	 * @param stRequirement - security requirement
	 * @param threats - set of threats (induced by a linktype)
	 * @return - true, if requirement is met
	 */
	public List<String> getViolations(
			final Stereotype stRequirement, final Set<String> threats) {
		List<String> violations = new ArrayList<String>();
		final String requirement = stRequirement.getName();
		if (requirement.equalsIgnoreCase("high")) {
			if (!(threats.isEmpty())) {
				violations.addAll(threats);
			}							
		} else if (requirement.equalsIgnoreCase("secrecy")) {
			if (threats.contains(READ)) {
				violations.add(READ);
			}
		} else if (requirement.equalsIgnoreCase("integrity") 
				&& threats.contains(INSERT)) {
			violations.add(INSERT);
		}
		return violations;
	}
	
	/**
	 * Checks if the link (or rather the linktype)
	 * complies with the requirements imposed by
	 * the dependency. 
	 * @param aLink - the link in question
	 * @param aDep - the dependency with requirements
	 * @return - true if the link complies with requirements
	 */
	public List<AnalysisMessage> 
	compliesWithRequirements(final CommunicationPath aLink, final Dependency aDep) {
		List<AnalysisMessage> errors = new ArrayList<AnalysisMessage>();
		for (Stereotype stRequirement : getRequirements(aDep)) {
			errors.addAll(compliesWithRequirement(aLink, stRequirement));
		}
		return errors;
	}

	public List<AnalysisMessage> compliesWithRequirements(final CommunicationPath aLink) {
		List<AnalysisMessage> errors = new ArrayList<AnalysisMessage>();
		for (Dependency dep : UMLDeploymentHelper.getAllDependencies(aLink)) {
			errors.addAll((compliesWithRequirements(aLink, dep)));
		}
		return errors;
	}

	/**
	 * Returns the set of threats imposed by
	 * attacker on the linktype.
	 * @param linktype - the linktype
	 * @param attacker - the attacker
	 * @return - set of threats
	 */
	private Set<String> getThreats(
			final String linktype, final String attacker) {
		HashSet<String> threats = new HashSet<String>();
		if (attacker.equalsIgnoreCase("default")) {
			if (linktype.equalsIgnoreCase("internet")) {
				threats.add(DELETE);
				threats.add(READ);
				threats.add(INSERT);
			} else if (linktype.equalsIgnoreCase("encrypted")) {
				threats.add(DELETE);
			}
		} else if (attacker.equalsIgnoreCase("insider")) {
			if (linktype.equalsIgnoreCase("internet") 
					|| linktype.equalsIgnoreCase("lan") 
					|| linktype.equalsIgnoreCase("encrypted")) {
				threats.add(DELETE);
				threats.add(READ);
				threats.add(INSERT);
			}
		} else if (attacker.equalsIgnoreCase("custom")) {
			if (linktype.equalsIgnoreCase("internet")) {
				threats.add(DELETE);
				threats.add(READ);
				threats.add(INSERT);
			} else if (linktype.equalsIgnoreCase("lan")) {
				threats.add(DELETE);
			} else if (linktype.equalsIgnoreCase("wire")) {
				threats.add(READ);
			}
		}
		return threats;
	}
	
	
	/**
	 * Gets the threats imposed by attacker on 
	 * stLinktype. If no attacker is given,
	 * it uses the model attacker.
	 * @param linktypeApplication - the linktype stereotype
	 * @param attacker - the attacker in question
	 * @return - the set of threats to the linktype
	 */
	public Set<String> getThreats(
			final StereotypeApplication linktypeApplication, final String attacker) {
		HashSet<String> threats = new HashSet<String>();
		if (linktypeApplication != null) {
			Stereotype stLinktype = linktypeApplication.getAppliedStereotype();
			if (SecureLinksHelper.isSecureLinksLinktype(stLinktype)) {
				String linktype = stLinktype.getName();
				if (attacker.equals("")) {
					String modelAttacker = getAttacker(linktypeApplication.getExtendedElement().getModel());
					threats.addAll(getThreats(linktype, modelAttacker));
				} else {
					threats.addAll(getThreats(linktype, attacker));
				}
			}
		}
		return threats;		
	}
	
	/**
	 * Gets the threats imposed by the model attacker
	 * on the given link.
	 * Convenience for getThreats(UMLStereotype)
	 * @param aLink - the link to check
	 * @return - the set of threats to the link/linktype
	 */
	public Set<String> getThreats(final CommunicationPath aLink) {
		return getThreats(
				getLinktype(aLink),
				getAttacker(aLink));
	}
	
	/**
	 * Returns the attacker of the model
	 * defined by the secure links stereotype.
	 * @param model - the model to search
	 * @return - the attacker type
	 */
	public String getAttacker(final Element element) {
		if (element == null) {
			throw new IllegalArgumentException("Tried to get attacker from a null object.");
		}
		String attacker = "";
		boolean foundSecureLinks = false;
		Element elementToCheck = element;
		do {
			if (UMLsecUtil.hasStereotype(elementToCheck, UMLsec.SECURE_LINKS)) {
				foundSecureLinks = true;
			} else {
				elementToCheck = elementToCheck.getOwner();
			}
		} while (!(foundSecureLinks || elementToCheck == null));
		if (foundSecureLinks) {
			List<String> adversaryValues = UMLsecUtil.getStringValues("adversary", UMLsec.SECURE_LINKS, elementToCheck);
			if (adversaryValues == null || adversaryValues.isEmpty() || adversaryValues.size() > 1) {
				return "default";
			} else {
				return adversaryValues.get(0);
			}
		}
		return attacker;
	}
	
	/**
	 * The given link is needed if the corresponding dependencies have at least
	 * one secure links requirement.
	 * @param aLink - link to check
	 * @return - true if the link is necessary
	 */
	public boolean linkIsNeeded(final CommunicationPath aLink) {
		for (Dependency dep : UMLDeploymentHelper.getAllDependencies(aLink)) {
			if (SecureLinksHelper.inSecureLinksScope(dep) && SecureLinksHelper.hasSecureLinksRequirements(dep)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * A link between two nodes is needed if at least one of the corresponding dependencies
	 * has a secure links requirement.
	 * @param nodes - nodes to check
	 * @return - true if a link between the nodes is needed
	 */
	public boolean linkIsNeeded(final List<Node> nodes) {
		if (nodes.size() != 2) {
			return false;
		}
		for (Dependency dep : UMLDeploymentHelper.getAllDependencies(nodes.get(0), nodes.get(1))) {
			if (SecureLinksHelper.inSecureLinksScope(dep) && SecureLinksHelper.hasSecureLinksRequirements(dep)) {
				return true;
			}			
		}
		return false;
	}
	
}
