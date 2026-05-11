/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *******************************************************************************/
package carisma.profile.umlsec.mltop10;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.common.util.EList;
import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Deployment;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;

import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;

/**
 * The implementation of the UMLsec profile. Provides easy access to the
 * profile's stereotypes and to applications of those.
 *
 */
public final class MLTop10Util {

	/**
	 * Hide constructor.
	 */
	private MLTop10Util() {
	}

	/**
	 * Checks, whether the Element <code>elem</code> has applied the Stereotype
	 * <code>stereo</code>.
	 * 
	 * @param elem   the Element to check
	 * @param stereo the Stereotype of interest
	 * @return true, if <code>stereo</code> is applied to <code>elem</code>
	 */
	public static boolean hasStereotype(final Element elem, final MLTop10 stereo) {
		for (Stereotype appliedStereo : elem.getAppliedStereotypes()) {
			if (stereo == null) {
				if (MLTop10.contains(appliedStereo)) {
					return true;
				}
			} else if (stereo.isEqual(appliedStereo)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Return the set containing the two nodes that are connected by the
	 * {@code path}.
	 * 
	 * @param path {@code CommunicationPath} of interest.
	 * @return Set of (probably two) nodes.
	 */
	public static Set<Node> getMemberNodes(CommunicationPath path) {
		Set<Node> nodes = new HashSet<Node>();
		for (Property end : path.getMemberEnds()) {
			if (end.getType() instanceof Node) {
				nodes.add((Node) end.getType());
			}
		}
		return nodes;
	}

	/**
	 * Returns the (first identified) {@code Node}, the {@code Artifact} is deployed
	 * to, either by being contained in the {@code Node} or by having a
	 * {@code Deployment} association with the {@code Node}.
	 * 
	 * TODO: consider single Artifact deployed to multiple Nodes
	 * 
	 * @param artifact    The {@code Artifact} that is deployed to a {@code Node}.
	 * @param deployments A {@code Set} of {@code Deployment} elements that should
	 *                    be checked for deployment associations.
	 * @return A {@code Node} element where the {@code Artifact} is deployed to or
	 *         {@code null} if no deployment could be identified.
	 */
	public static Node getDeploymentNode(Artifact artifact, Set<Deployment> deployments) {
		if (artifact.eContainer() instanceof Node) {
			return (Node) artifact.eContainer();
		}
		for (Deployment dep : deployments) {
			if ((dep.getDeployedArtifacts().get(0) == artifact) && (dep.getClients().get(0) instanceof Node)) {
				return (Node) dep.getClients().get(0);
			}
		}
		return null;
	}

	/**
	 * Returns a {@code Set} of {@code Element} elements contained in the
	 * {@code Element} that are instances of {@type} and have the stereotype
	 * {@code stereo}. The set can also contain the {@code Element} itself, if it
	 * matches the class and stereotype criteria.
	 * 
	 * @param <T>    The type of the elements in the returned list.
	 * @param model  The model that contains the element candidates.
	 * @param type   The class (including subclasses) the returned elements are
	 *               instances of.
	 * @param stereo The stereotype the returned elements have to have.
	 * @return Set of Elements contained in {@code Element} of class
	 *         {@code Class<T>} with stereotype {@code MLTop10}
	 */
	public static <T extends Element> Set<T> getStereotypedElements(final Element model, final Class<T> type,
			final MLTop10 stereo) {
		Set<T> elements = new HashSet<T>(UMLHelper.getAllElementsOfType(model, type));
		elements.removeIf(e -> !(hasStereotype(e, stereo)));
		if ((type.isInstance(model)) && hasStereotype(model, stereo)) {
			elements.add(type.cast(model));
		}
		return elements;
	}

	/**
	 * Returns all elements contained in the given {@code element} which are of the
	 * given {@code type}.
	 * 
	 * @param <T>     Type of interest.
	 * @param element The element to look for elements of type {@code Class<T>}
	 * @param type    the type of interest.
	 * @return Set of elements matching the {@code type}, potentially including
	 *         {@code element} itself.
	 */
	public static <T> Set<T> getAllElementsOfType(Element element, Class<T> type) {
		return new HashSet<T>(UMLHelper.getAllElementsOfType(element, type));
	}

	/**
	 * Check whether the {@code attribute} of the {@code stereotype} attached to
	 * {@code element} is set to {@code true}.
	 * 
	 * @param element    the element to look for the stereotype.
	 * @param stereotype the stereotype containing the attribute of interest.
	 * @param attribute  the attribute of interest.
	 * @return true, if the attribute is set to 'true'.
	 */
	public static boolean isTaggedValueTrue(Element element, MLTop10 stereotype, String attribute) {
		List<Object> tag = MLTop10Util.getTaggedValues(attribute, stereotype, element);
		return !(tag != null && !tag.isEmpty() && tag.get(0).equals(false));
	}

	/**
	 * Returns a list of the tag values of the UMLsec stereotype at the given
	 * element stereoParent.
	 * 
	 * @param tagName      - The name of the tag to read the values of
	 * @param stereo       - the stereotype with the tag
	 * @param stereoParent - the element the stereotype is applied to
	 * @return - a list of tag values; empty if the stereotype doesn't have the tag
	 */
	@SuppressWarnings("unchecked")
	private static List<Object> getTaggedValues(final String tagName, final MLTop10 stereo,
			final Element stereoParent) {
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

	/**
	 * Returns the StereotypeApplication of the given stereotype if it is applied.
	 * 
	 * @param stereo  - stereotype
	 * @param element - element
	 * @return stereotypeApplication if found, null otherwise
	 */
	private static StereotypeApplication getStereotypeApplication(final Element element, final MLTop10 stereo) {
		for (StereotypeApplication stereoApp : getStereotypeApplications(element, stereo)) {
			MLTop10 type = MLTop10.getValue(stereoApp.getAppliedStereotype().getName());
			if (type.equals(stereo)) {
				return stereoApp;
			}
		}
		return null;
	}

	/**
	 * Returns the UMLsec stereotype applications to the given element.
	 * 
	 * @param element - the element to inspect
	 * @return - list of UMLsec stereotype applications
	 */
	private static List<StereotypeApplication> getStereotypeApplications(final Element element,
			final MLTop10 stereotype) {
		List<StereotypeApplication> result = new ArrayList<StereotypeApplication>();
		for (Stereotype stereo : element.getAppliedStereotypes()) {
			if (MLTop10.contains(stereo)) {
				result.add(new StereotypeApplication(stereo, element));
			}
		}
		return result;
	}

}