package carisma.evolution;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;

public class CopyElement extends AdditiveElement {
	
	private EObject receivingElement = null;
	
	/**
	 * The changed values of the new element.
	 */
	private Map<String,Object> changedValues = null;
	
	public CopyElement(final EObject changeTarget, final EObject newReceiver) {
		super(changeTarget);
		this.changedValues = new HashMap<>();
		this.receivingElement = newReceiver;
	}
	
	public EObject getReceivingElement() {
		return this.receivingElement;
	}
	
	public Map<String, Object> getChangedValues() {
		return Collections.unmodifiableMap(this.changedValues);
	}
	
	/**
	 * Replaces the property values of the added element with
	 * a new set of property values.
	 * @param newValues - new property values
	 */
	public void replaceChangedValues(final Map<? extends String,? extends Object> newValues) {
		this.changedValues.clear();
		this.changedValues.putAll(newValues);
	}
	
	public boolean addChangedValuePair(final String newKey, final Object newValue) {
		this.changedValues.put(newKey, newValue);
		return true;
	}
	
	public boolean removeChangedValuePair(final String oldKey) {
		this.changedValues.remove(oldKey);
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
		if (other instanceof CopyElement) {
			CopyElement otherElement = (CopyElement) other;
			if (
					(
							(otherElement.getTarget() == null && this.getTarget() == null)
							|| otherElement.getTarget().equals(this.getTarget())
					) && 
					(
							otherElement.getChangedValues().equals(this.getChangedValues())
					) &&
					(
							(otherElement.getReceivingElement() == null && this.getReceivingElement() == null)
							|| otherElement.getReceivingElement().equals(this.getReceivingElement())
					)
				) {
				return true;
			}
		}
		return false;
	}
}
