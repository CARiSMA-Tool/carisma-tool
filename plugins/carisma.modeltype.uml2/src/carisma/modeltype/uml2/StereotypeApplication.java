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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.UMLPackage;

import carisma.core.util.EObjectUtil;


/**
 * This class models the application of
 * a stereotype to an element to enable
 * changes of applications. 
 * @author Daniel Warzecha
 *
 */
public class StereotypeApplication extends EObjectImpl {
	
	/**
	 * Basic value to compute the HashCode.
	 */
	private static final int HASH_CODE_BASIS = 17;
	
	/**
	 * Value of a factor to compute the HashCode.
	 */
	private static final int HASH_CODE_FACTOR = 37;
	
	private Stereotype stereotype = null;
	
	private Element extendedElement = null;
	
	public StereotypeApplication(final Stereotype newStereotype, final Element newExtendedElement) {
		this.stereotype = newStereotype;
		this.extendedElement = newExtendedElement;
	}
	
	public Stereotype getAppliedStereotype() {
		return this.stereotype;
	}
	
	public String getQualifiedStereotypeName() {
		return this.stereotype.getQualifiedName();
	}
	
	public String getStereotypeName() {
		return this.stereotype.getName();
	}
	
	public Model getModel() {
		return this.extendedElement.getModel();
	}
	
	/**
	 * Simple method wrapping getStereotypeName() (ease of use)
	 * @return - the name of the applied stereotype
	 */
	public String getName() {
		return getStereotypeName();
	}
	
	public String getExtendedElementName() {
		if (this.extendedElement instanceof NamedElement) {
			return ((NamedElement) this.extendedElement).getName();
		}
		return "[Unnamed Element]";
	}
	
	public Element getExtendedElement() {
		return this.extendedElement;
	}
	
	@Override
	public EClass eClass()
	{
		return UMLPackage.eINSTANCE.getStereotype();
	}
	
	/** 
	 * Collects all changeable tagged values for this stereotype application.
	 * @return - list of tagged values
	 */
	public List<TaggedValue> getTaggedValues() {
		List<TaggedValue> tagValues = new ArrayList<>();
		for (Property tag : this.stereotype.getAllAttributes()) {
			if (!tag.isDerived()) {
				String [] parts = tag.getName().split("_");
				if ((parts[0].equals("base") && parts.length > 1) 
						&& ((UMLPackage.eINSTANCE.getEClassifier(parts[1]) instanceof EClass))) {
					continue;
				}
				tagValues.add(new TaggedValue(tag, this));						
			}
		}
		return tagValues;
	}
	
	/**
	 * Tells if the Stereotype of the application has the named tag defined.
	 * @param name - the name of the defined tag
	 * @return - true if the tag is defined at the Stereotype
	 */
	public boolean hasDefinedTag(final String name) {
		if (getTaggedValue(name) != null) {
			return true;
		}
		return false;
	}
	
	/**
	 * Returns the tagged value, if changeable and existent, with the given name.
	 * @param name
	 * @return
	 */
	public TaggedValue getTaggedValue(final String name) {
		for (Property tag : this.stereotype.getAllAttributes()) {
			if (!tag.isDerived() && tag.getName().equalsIgnoreCase(name)) {
				return new TaggedValue(tag,this);
			}
		}
		return null;
	}
	
	/**
	 * Checks if the application has a non-default value for the given tag.
	 * @param tagName - the tag name
	 * @return - false if value is default or tag doesn't exist
	 */
	public boolean hasTagValue(final String tagName) {
		for (Property tag : this.stereotype.getAllAttributes()) {
			if ((!tag.isDerived() && tag.getName().equalsIgnoreCase(tagName))
					&& (this.extendedElement.hasValue(this.stereotype, tagName))) {
				return true;
			}
		}
		return false;
	}
	
	
	@Override
	public boolean equals(Object other) {
		if (other == this) {
			return true;
		}
		if (other instanceof StereotypeApplication) {
			StereotypeApplication otherApplication = (StereotypeApplication) other;
			if (otherApplication.getAppliedStereotype().equals(this.stereotype) 
					&& otherApplication.getExtendedElement().equals(this.extendedElement)) {
				return true;
			}
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		int result = HASH_CODE_BASIS;
		result = HASH_CODE_FACTOR * result + ((this.stereotype == null) ? 0 : this.stereotype.hashCode());
		result = HASH_CODE_FACTOR * result + ((this.extendedElement == null) ? 0 : this.extendedElement.hashCode());
		
		return result;
	}
	
	@Override
	public String toString() {
		return "StereotypeApplication of <<" + this.stereotype.getName() + ">> at " + EObjectUtil.getTypeAndName(this.extendedElement) + ".";
	}
	
	@Override
	public org.eclipse.emf.ecore.EObject eContainer() {
		return getExtendedElement();
	}
}
