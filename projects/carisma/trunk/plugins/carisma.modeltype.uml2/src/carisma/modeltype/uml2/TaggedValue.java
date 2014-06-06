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
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.uml2.uml.DataType;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.UMLPackage;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

/**
 * To change TaggedValues, this class is used to store
 * in delta elements.
 * @author Daniel Warzecha
 *
 */
public class TaggedValue extends EObjectImpl {
	
	/**
	 * Basic value to compute the HashCode.
	 */
	private static final int HASH_CODE_BASIS = 17;
	
	/**
	 * Value of a factor to compute the HashCode.
	 */
	private static final int HASH_CODE_FACTOR = 37;
	
	private Property tag = null;
	
	private StereotypeApplication correspondingApplication = null;
	
	public TaggedValue(final Property newProperty, final StereotypeApplication newApplication) {
		tag = newProperty;
		correspondingApplication = newApplication;
	}
	
	public Property getTag() {
		return tag;
	}
	
	public StereotypeApplication getCorrespondingApplication() {
		return correspondingApplication;
	}
	
	public boolean isMultivalued() {
		return tag.isMultivalued();
	}
	
	public Stereotype getStereotype() {
		return correspondingApplication.getAppliedStereotype();
	}
	
	public String getQualifiedStereotypeName() {
		return correspondingApplication.getAppliedStereotype().getQualifiedName();
	}
	
	public String getStereotypeName() {
		return correspondingApplication.getAppliedStereotype().getName();
	}
	
	public Element getElement() {
		return correspondingApplication.getExtendedElement();
	}
	
	public String getName() {
		return tag.getName();
	}
	
	@Override
	public EClass eClass()
	{
		return UMLPackage.eINSTANCE.getProperty();
	}

	public Object getValue() {
		checkTagInitialization();
		Element extendedElement = correspondingApplication.getExtendedElement();
		Stereotype appliedStereo = correspondingApplication.getAppliedStereotype();
		return extendedElement.getValue(appliedStereo, this.getName());
	}
	
	/**
	 * Returns the list of String values of the tag.
	 * @return - list of found String values
	 */	
	public List<String> getStringValues() {
		List<String> valueList = new ArrayList<String>();
		Object tagValueObject = getValue();
		if (tagValueObject instanceof EList<?>) {
			for (Object entry : (EList<?>) tagValueObject) {
				if (entry instanceof String) {
					valueList.add((String) entry);
				}
			}
		} else if (tagValueObject instanceof String) {
			valueList.add((String) tagValueObject);
		}
		return Collections.unmodifiableList(valueList);
	}
	
	@SuppressWarnings("unchecked")
	public void setValue(Object newValue) {
		if (getTag().isMultivalued()) {
			if (newValue instanceof List<?>) {
				setTagValue(newValue);
			} else {
				List<Object> list = (List<Object>) getValue();
				list.add(newValue);				
			}
		} else {
			setTagValue(newValue);
		}
	}
	
	@SuppressWarnings("unchecked")
	public void removeValue() {
		checkTagInitialization();
		if (getTag().isMultivalued()) {
			List<Object> list = (List<Object>) getValue();
			list.clear();
		} else {
			correspondingApplication.getExtendedElement().setValue(correspondingApplication.getAppliedStereotype(), getName(), null);
		}
	}
	
	@SuppressWarnings("unchecked")
	public void removeValue(final Object oldValue) {
		checkTagInitialization();
		if (getTag().isMultivalued()) {
			List<Object> list = (List<Object>) getValue();
			list.remove(oldValue);
		} else {
			correspondingApplication.getExtendedElement().setValue(correspondingApplication.getAppliedStereotype(), getName(), null);
		}
			
	}
	
	@SuppressWarnings("unchecked")
	public void removeValue(final int index) {
		checkTagInitialization();
		if (getTag().isMultivalued()) {
			List<Object> list = (List<Object>) getValue();
			list.remove(index);
		} else {
			correspondingApplication.getExtendedElement().setValue(correspondingApplication.getAppliedStereotype(), getName(), null);
		}
	}
	
	public boolean hasValue() {
		if (correspondingApplication.hasTagValue(tag.getName())) {
			return true;			
		}
		return false;
	}
	
	private void checkTagInitialization() {
		Element extendedElement = correspondingApplication.getExtendedElement();
		Stereotype appliedStereo = correspondingApplication.getAppliedStereotype();
		Object value = extendedElement.getValue(appliedStereo, this.getName());
		if (value == null) {
			DataType type = getTag().getDatatype();
			Logger.log(LogLevel.DEBUG, "Datatype von null-Liste: " + type);
		}
	}
	
	private void setTagValue(final Object newValue) {
		checkTagInitialization();
		correspondingApplication.getExtendedElement().setValue(correspondingApplication.getAppliedStereotype(), getName(), newValue);
	}

	@Override
	public boolean equals(Object other) {
		if (other == this) {
			return true;
		}
		if (other instanceof TaggedValue) {
			TaggedValue otherTagValue = (TaggedValue) other;
			if (otherTagValue.getTag().equals(tag) 
					&& otherTagValue.getCorrespondingApplication().equals(correspondingApplication)) {
				return true;
			}
		}
		return false;
	}
	
	
	@Override
	public int hashCode() {
		int result = HASH_CODE_BASIS;
		result = HASH_CODE_FACTOR * result + ((this.tag == null) ? 0 : this.tag.hashCode());
		result = HASH_CODE_FACTOR * result + ((this.correspondingApplication == null) ? 0 : this.correspondingApplication.hashCode());
		return result;
	}
	
	@Override
	public String toString() {
		return "TaggedValue {" + tag.getName() + "=" + getValue() + "}";
	}

	@Override
	public org.eclipse.emf.ecore.EObject eContainer() {
		return getCorrespondingApplication();
	}
}
