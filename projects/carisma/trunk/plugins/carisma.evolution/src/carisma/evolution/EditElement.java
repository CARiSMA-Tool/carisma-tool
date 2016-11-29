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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;

/**
 * A delta element that describes the modification
 * of the target element. 
 * @author Johannes Kowald
 */
public class EditElement extends DeltaElement {
	
	/**
	 * the further content of the new element.
	 */
	private Map<String, Object> values;
	
	
	/**
	 * constructor for a new EditElement.
	 * @param newTarget - the element that will be edited
	 */
	public EditElement(final EObject newTarget) {
		super(newTarget);
		this.values = new HashMap<>();
	}
	
	/**
	 * returns the content of the EditElement.
	 * @return - the content
	 */
	public final Map<String, Object> getValues() {
		return Collections.unmodifiableMap(this.values);
	}
	
	/**
	 * Replaces the property values of the added element with
	 * a new set of property values.
	 * @param newValues - new property values
	 */
	public final void replaceValues(final Map<? extends String, ? extends Object> newValues) {
		this.values.clear();
		this.values.putAll(newValues);
	}
	
	/**
	 * adds a change to the EditElement.
	 * @param newKey - the tag that should be changed
	 * @param newValue - the new value of the tag
	 * @return - returns true if the adding was succesfull
	 */
	public final boolean addKeyValuePair(final String newKey, final Object newValue) {
		this.values.put(newKey, newValue);
		return true;
	}
	
	/**
	 * removes a change from the EditElement.
	 * @param oldKey - the tag that shouldn't be changed
	 * @return - true if the removeing was successful
	 */
	public final boolean removeKeyValuePair(final String oldKey) {
		this.values.remove(oldKey);
		return true;
	}
	
	/**
	 * edits a change in the EditElement.
	 * @param oldKey - the tag that should get an other change.
	 * @param newValue - the now new value
	 * @return - false if the old Value was null, true otherwise
	 */
	public final boolean editKeyValuePair(final String oldKey, final Object newValue) {
		Object oldValue = this.values.remove(oldKey);
		this.values.put(oldKey, newValue);
		if (oldValue == null) {
			return false;
		}
		return true;
	}

	/**
	 * A delta element is equal to another delta element,
	 * if and only if all attributes are equal.
	 * @param other - element to compare with
	 * @return - true, if otherElement equals this element
	 */
	@Override
	public final boolean equals(final Object other) {
		if (other == this) {
			return true;
		}
		if (other instanceof EditElement) {
			EditElement otherElement = (EditElement) other;
			if ((otherElement.getTarget() == null && this.getTarget() == null) 
				|| otherElement.getTarget().equals(this.getTarget())
				&& otherElement.getValues().equals(this.getValues())) {
				return true;
			}
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		int result = 17;
		result = 37*result + ((this.getTarget() == null) ? 0 : this.getTarget().hashCode());
		return result;
	}
	
	@Override
	public final String toString() {
		StringBuilder buf = new StringBuilder();
		
		if (this.getTarget() != null) {
			buf.append("Editing "
					+ EObjectUtil.getTypeAndName(this.getTarget())
					+ ": ");
		}
		buf.append(" (");
		for (String key : this.values.keySet()) {
			Object value = this.values.get(key);
			buf.append(key + "=");
			if (value instanceof EObject) {
				EObject valObj = (EObject) value;
				buf.append(EObjectUtil.getTypeAndName(valObj));
			} else if (value != null){
				buf.append(value.toString());
			} else {
			    buf.append("null");
			}
			buf.append(",");
		}
		buf.deleteCharAt(buf.length() - 1);
		buf.append(")");
		return buf.toString();
	}
}
