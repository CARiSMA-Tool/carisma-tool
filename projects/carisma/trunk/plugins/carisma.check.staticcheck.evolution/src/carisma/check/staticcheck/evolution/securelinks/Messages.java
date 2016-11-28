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
package carisma.check.staticcheck.evolution.securelinks;

import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Stereotype;

import carisma.check.staticcheck.securelinks.SecureLinksHelper;
import carisma.core.util.EObjectUtil;
import carisma.evolution.AddElement;
import carisma.evolution.DeltaElement;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLDeploymentHelper;


final class Messages {
       
    /**
     * Constant String for the name of the key 'name'.
     */
    private static final String NAME = "name";
    
    /**
     * Constant String.
     */
    private static final String AT = " at ";
    
    /**
     * Hiding constructor.
     */
	private Messages() {
	}
	
	static String linkDeletionAllowed(final DeltaElement delElement) {
		CommunicationPath deletedLink = (CommunicationPath) delElement.getTarget();
		return "Deletion of " 
			+ EObjectUtil.getTypeAndName(deletedLink)
			+ " is allowed.";
	}
	
	static String linktypeDeletionAllowed(final DeltaElement delElement) {
		StereotypeApplication stapp = (StereotypeApplication) delElement.getTarget();
		return "Deletion of Linktype-"
				+ EObjectUtil.getTypeAndName(stapp.getAppliedStereotype())
				+ AT
				+ EObjectUtil.getTypeAndName(stapp.getExtendedElement())
				+ " is allowed.";
	}
	
	static String linktypeReplacementWrong(final DeltaElement delElement, final CommunicationPath postLink, final Stereotype postLinktype) {
		StereotypeApplication stapp = (StereotypeApplication) delElement.getTarget();
		Stereotype preLinktype = stapp.getAppliedStereotype();
		return "Deletion of Linktype-"
				+ EObjectUtil.getTypeAndName(preLinktype)
				+ AT
				+ EObjectUtil.getTypeAndName(postLink)
				+ " violates security. The replacement Linktype-"
				+ EObjectUtil.getTypeAndName(postLinktype)
				+ " is inappropriate.";
	}
	
	static String linkDeletionNotAllowed(final DeltaElement delElement) {
		CommunicationPath deletedLink = (CommunicationPath) delElement.getTarget();
		List<Node> linkNodes = UMLDeploymentHelper.getNodes(deletedLink);
		StringBuffer buf = new StringBuffer();
		for (Iterator<Node> nodeIt = linkNodes.iterator(); nodeIt.hasNext();) {
			Node node = nodeIt.next();
			buf.append(node.getName());
			if (nodeIt.hasNext()) {
				buf.append(",");
			}
		}
		return "The replacement of the link or linktype on "
			+ EObjectUtil.getTypeAndName(deletedLink)
			+ " (Nodes: "
			+ buf.toString()
			+ ") violates model security. "
			+ "Use an appropriate linktype.";
		
	}	
	
	static String linktypeDeletionNotAllowed(final DeltaElement delElement) {
		StereotypeApplication delApp = (StereotypeApplication) delElement.getTarget();
		CommunicationPath commPath = (CommunicationPath) delApp.getExtendedElement();
		Stereotype appliedStereo = delApp.getAppliedStereotype();
		List<Node> linkNodes = UMLDeploymentHelper.getNodes(commPath);
		StringBuffer buf = new StringBuffer();
		for (Iterator<Node> nodeIt = linkNodes.iterator(); nodeIt.hasNext();) {
			Node node = nodeIt.next();
			buf.append(node.getName());
			if (nodeIt.hasNext()) {
				buf.append(",");
			}
		}
		return "The replacement of Linktype-" 
			+ EObjectUtil.getTypeAndName(appliedStereo)
			+ " on "
			+ EObjectUtil.getTypeAndName(commPath)
			+ " (Nodes: "
			+ buf.toString()
			+ ") violates model security. "
			+ "Use an appropriate linktype.";
		
	}	
	
	static String secureLinksDeletion(final DeltaElement delElement) {
		return "Deletion of secure links stereotype " 
			+ "trivializes the check.";
	}
	
	static String otherDeletion(final DeltaElement delElement) {
		EObject target = delElement.getTarget();
		if (target instanceof Element) {
			return "Deleted " 
			+ EObjectUtil.getTypeAndName(target);
		} else if (target instanceof StereotypeApplication) {
			StereotypeApplication stapp = (StereotypeApplication) target;
			return "Deleted Application of "
			+ EObjectUtil.getTypeAndName(stapp.getAppliedStereotype())
			+ " to "
			+ EObjectUtil.getTypeAndName(stapp.getExtendedElement())
			+ ".";
		} else if (target instanceof TaggedValue) {
			TaggedValue tv = (TaggedValue) target;
			return "Deleted TaggedValue "
					+ tv.getName()
					+ " at application of "
					+ EObjectUtil.getTypeAndName(tv.getCorrespondingApplication().getAppliedStereotype())
					+ AT
					+ EObjectUtil.getTypeAndName(tv.getCorrespondingApplication().getExtendedElement())
					+ ".";
		}
		return "";
	}
	
	static String adversaryTagDeleted(final DeltaElement delElement) {
		return  "Deletion of non-default adversary " 
			+ "changes the adversary to default."
			+ "A complete analysis is needed.";	
	}
	
	static String adversaryTagAdded() {
		return "UMLchangeSecureLinksCheck::" 
				+ "processAddElements:" 
				+ "Addition of adversary tag " 
				+ "calls for a complete model check.";
	}
	
	static String otherTagAddition(final AddElement add) {
		String tagName = (String) add.getValues().get(NAME);
		String tagValue = (String) add.getValues().get("value");
		StereotypeApplication stapp = (StereotypeApplication) add.getTarget();
		String stereoName = EObjectUtil.getTypeAndName(stapp.getAppliedStereotype());
		String extendedElem = EObjectUtil.getTypeAndName(stapp.getExtendedElement());
		return "Addition of TaggedValue {" + tagName + "=" + tagValue + "}"
			+ AT
			+ extendedElem
			+ " ("
			+ stereoName
			+ ") valid.";		
	}
	
	static String newRequirementNotMet(final AddElement addElement, final CommunicationPath commPath, final Stereotype postLinktype) {
		String requirement = (String) addElement.getValues().get(NAME);
		EObject target = addElement.getTarget();
		return "Addition of requirement "
				+ requirement
				+ " to "
				+ EObjectUtil.getTypeAndName(target)
				+ " is not met by Communication Path "
				+ EObjectUtil.getTypeAndName(commPath)
				+ " having linktype "
				+ EObjectUtil.getTypeAndName(postLinktype)
				+ ".";
	}
	
	static String newLinktypeNotAppropriate(final AddElement addElement, final Dependency dep) {
		String linktype = (String) addElement.getValues().get(NAME);
		return "New Linktype "
				+ linktype
				+ " not appropriate in regards to "
				+ EObjectUtil.getTypeAndName(dep)
				+ " with requirements "
				+ getRequirementsString(dep)
				+ ".";
	}
	
	static String newLinkNoLinktype(final AddElement addElement, final CommunicationPath commPath, final Dependency dep) {
		return "New "
				+ EObjectUtil.getTypeAndName(commPath)
				+ " has no linktype. "
				+ EObjectUtil.getTypeAndName(dep)
				+ " has requirements "
				+ getRequirementsString(dep)
				+ " and therefore needs defined linktype.";
	}
	
	static String newDeploymentNoLink(final AddElement addElement, final Node location, final Node targetNode, final Artifact deployedArtifact, final Dependency dep) {
		String deploymentName = (String) addElement.getValues().get(NAME);
		return "No link between "
				+ EObjectUtil.getTypeAndName(location)
				+ " and "
				+ EObjectUtil.getTypeAndName(targetNode)
				+ " after adding Deployment "
				+ deploymentName
				+ ". "
				+ EObjectUtil.getTypeAndName(dep)
				+ " with requirements "
				+ getRequirementsString(dep)
				+ " needs link.";
	}
	
	static String newDeploymentLinkNotAppropriate(final AddElement addElement, final Node location, final Dependency dep, final CommunicationPath commPath, final Stereotype linktype) {
		String deploymentName = (String) addElement.getValues().get(NAME);
		return "Adding deployment "
				+ deploymentName
				+ " to "
				+ EObjectUtil.getTypeAndName(location)
				+ " leads to inappropriate linktype "
				+ EObjectUtil.getTypeAndName(linktype)
				+ " on "
				+ EObjectUtil.getTypeAndName(commPath)
				+ " in regards to "
				+ EObjectUtil.getTypeAndName(dep)
				+ " with requirements "
				+ getRequirementsString(dep)
				+ ".";
	}
	
	static String otherElementAddition(final AddElement addElement) {
		EObject target = addElement.getTarget();
		return "Added "
				+ addElement.getMetaClass().getName()
				+ " '"
				+ (String) addElement.getValues().get(NAME)
				+ "' to "
				+ EObjectUtil.getTypeAndName(target);
	}
	private static String getRequirementsString(final Dependency dep) {
		StringBuffer sb = new StringBuffer();
		sb.append("{");
		for (Stereotype requirement : SecureLinksHelper.getAllRequirements(dep)) {
			sb.append(requirement.getName());
			sb.append(",");
		}
		sb.deleteCharAt(sb.lastIndexOf(","));
		sb.append("}");
		return sb.toString();
	}
}
