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
package carisma.evolution;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

public class AddElement extends AdditiveElement {
	/**
	 * The metaclass of the added element.
	 */
	private EClass metaClass;
	/**
	 * The property values of the new element.
	 */
	private Map<String, Object> values;
	/**
	 * The further content of the new element.
	 */
	private List<AddElement> content;
	/**
	 * If the element is a contained element,
	 * this describes the parent element.
	 */
	private AdditiveElement parent;
	
	public AddElement(
			final EObject newTarget,
			final EClass newMetaClass,
			final AdditiveElement newParent) {
		super(newTarget);
		if (newMetaClass == null) {
			throw new IllegalArgumentException("Tried to initialize AddElement with null Metaclass!");
		}
		this.metaClass = newMetaClass;
		this.values = new HashMap<>();
		this.content = new ArrayList<>();
		this.parent = newParent;
	}
	
	public EClass getMetaClass() {
		return this.metaClass;
	}
	
	public Map<String, Object> getValues() {
		return Collections.unmodifiableMap(this.values);
	}
	
	/**
	 * Replaces the property values of the added element with
	 * a new set of property values.
	 * @param newValues - new property values
	 */
	public void replaceValues(final Map<? extends String,? extends Object> newValues) {
		if (newValues == null) {
			throw new IllegalArgumentException("Tried to replace values map with null map.");
		}
		this.values.clear();
		this.values.putAll(newValues);
	}
	
	public boolean addKeyValuePair(final String newKey, final Object newValue) {
		if (newKey == null || newKey.isEmpty()) {
			throw new IllegalArgumentException("Given new key is null or empty.");
		}
		this.values.put(newKey, newValue);
		return true;
	}
	
	public boolean removeKeyValuePair(final String oldKey) {
		this.values.remove(oldKey);
		return true;
	}

	public List<AddElement> getContent() {
		return Collections.unmodifiableList(this.content);
	}
	/**
	 * Returns the set of all added elements directly or
	 * indirectly contained in this added element,
	 * including the element itself.
	 * @return - the set of all added elements
	 */
	public List<AddElement> getAllAddedElements() {
		List<AddElement> allAddedElements = new ArrayList<>();
		allAddedElements.add(this);
		for (AddElement containedElem : this.content) {
			allAddedElements.addAll(containedElem.getAllAddedElements());
		}
		return allAddedElements;
	}
	/**
	 * After creating the described element, the target of the contained elements
	 * can be updated with the new target element.
	 * @param newElement - new created element represented by this add element
	 */
	public void updateContent(final EObject newElement) {
// TODO: validate that the newElement is the element described by this AddElement
		for (AddElement addElem : this.content) {
			addElem.setTarget(newElement);
		}
	}
	
	/**
	 * Replaces the content of the added element with
	 * a new set of contained elements.
	 * @param newContent - new contained elements
	 */
	public void replaceContent(final List<AddElement> newContent) {
		for (AddElement c : this.content) {
			c.setParent(null);
		}
		this.content.clear();
		for (AddElement c : newContent) {
			addContainedElement(c);
		}
	}
	
	public boolean addContainedElement(final AddElement newElement) {
		if (newElement == null) {
			throw new IllegalArgumentException("Tried to add null AddElement to the content.");
		}
		if (!this.content.contains(newElement)) {
			newElement.setParent(this);
			return this.content.add(newElement);
		}
		return false;
	}

	public boolean addContainedElements(final List<AddElement> newContainedElements) {
		boolean alreadyInContent = false;
		for (AddElement de : newContainedElements) {
			if (!addContainedElement(de)) {
				alreadyInContent = true;
			}
		}
		return alreadyInContent;
	}

	public boolean removeContainedElement(final AddElement oldElement) {
		oldElement.setParent(null);
		return this.content.remove(oldElement);
	}
	
	void setParent(AdditiveElement newParent) {
		this.parent = newParent;
	}

	public AdditiveElement getParent() {
		return this.parent;
	}

	/**
	 * A delta element is equal to another delta element,
	 * if and only if all attributes are equal.
	 * @param otherElement - element to compare with
	 * @return - true, if otherElement equals this element
	 */
	@Override
	public boolean equals(final Object other) {
		if (other == this) {
			return true;
		}
		if (other instanceof AddElement) {
			AddElement otherElement = (AddElement) other;
			
			if (
				(	
					(otherElement.getMetaClass() == null && this.getMetaClass() == null) 
					|| otherElement.getMetaClass().equals(this.metaClass)
				)
				&& 
				(
					(otherElement.getTarget() == null && this.getTarget() == null) 
					|| otherElement.getTarget().equals(this.getTarget())
				)
					&& otherElement.getValues().equals(this.values)
					&& otherElement.getContent().equals(this.content)) {
				return true;
			}
		}
		return false;
	}

// FIXME: Problems with hashCode override
//	@Override
//	public int hashCode() {
//		int result = 17;
//		result = 37*result + ((this.metaClass == null) ? 0 : this.metaClass.hashCode());
//		result = 37*result + ((this.getTarget() == null) ? 0 : this.getTarget().hashCode());
//		result = 37*result + ((this.values == null) ? 0 : this.values.hashCode());
//		result = 37*result + ((this.content == null) ? 0 : this.content.hashCode());			
//		return result;
//	}
	
	@Override
	public String toString() {
		StringBuilder buf = new StringBuilder();
		
		if (this.getTarget() != null) {
			buf.append("Adding to "
					+ EObjectUtil.getTypeAndName(this.getTarget())
					+ ": ");
		}
		buf.append(this.metaClass.getName());
		buf.append(" (");
		for (String key : this.values.keySet()) {
			Object value = this.values.get(key);
			buf.append(key + "=");
			if (value instanceof EObject) {
				EObject valObj = (EObject) value;
				buf.append(EObjectUtil.getTypeAndName(valObj));
			} else {
				if (value != null) {					
					buf.append(value.toString());
				}
			}
			buf.append(",");
		}
		if (!this.values.isEmpty()) {
			buf.deleteCharAt(buf.length() - 1);			
		}
		buf.append(")");
		if (!this.content.isEmpty()) {
			buf.append(", containing " + this.content.toString());
		}
		return buf.toString();
	}
}
