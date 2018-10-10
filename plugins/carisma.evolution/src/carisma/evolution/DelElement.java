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
import java.util.List;

import org.eclipse.emf.ecore.EObject;
/**
 * A DelElement is a delta element that describes the
 * deletion of an element in a model. If an element
 * is deleted, there can be accompanying deletions
 * of other model elements to maintain the validity
 * of the model. 
 * @author Daniel Warzecha
 *
 */
public class DelElement extends DeltaElement {
	private List<EObject> accompanyingDeletions = null;
	
	public DelElement(final EObject newTarget) {
		super(newTarget);
		this.accompanyingDeletions = new ArrayList<>();
	}
	
	public List<EObject> getAccompanyingDeletions() {
		return this.accompanyingDeletions;
	}
	/**
	 * Replaces the accompanying deletions with a new set.
	 * @param newDeletions - new accompanying deletions
	 */
	public void replaceAccompanyingDeletions(final List<EObject> newDeletions) {
		if (newDeletions != null) {
			this.accompanyingDeletions.clear();
			for (EObject accDel : newDeletions) {
				addDeletion(accDel);
			}
		}
	}
	/**
	 * Adds an element that is also deleted when the target element
	 * is removed from the model.
	 * @param deletedElement - the element that is removed
	 * @return - true if addition of deleted element successful
	 */
	public boolean addDeletion(final EObject deletedElement) {
		if (!this.accompanyingDeletions.contains(deletedElement)) {
			return this.accompanyingDeletions.add(deletedElement);
		}
		return false;
	}
	/**
	 * Adds a set of elements that are also deleted
	 * when the target element is removed from the model.
	 * @param newDeletions - the set of also deleted elements
	 * @return - true if all elements could be added
	 */
	public boolean addDeletions(final List<EObject> newDeletions) {
		if (newDeletions != null) {
			for (EObject newDeletion : newDeletions) {
				if (!this.accompanyingDeletions.contains(newDeletion)) { 
					addDeletion(newDeletion);
				}
			}
		}
		return true;
	}

	public boolean removeDeletion(final EObject oldDeletion) {
		return this.accompanyingDeletions.remove(oldDeletion);
	}
	
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append("Deleting ");
		buf.append(EObjectUtil.getTypeAndName(this.getTarget()));
		if (!this.accompanyingDeletions.isEmpty()) {
			buf.append(". Also deleting:");
			for (EObject deletion : this.accompanyingDeletions) {
				buf.append("  ");
				buf.append(EObjectUtil.getTypeAndName(deletion));
				buf.append(",");
			}
			buf.deleteCharAt(buf.length() - 1);
		}
		return buf.toString();
	}
	
	@Override
	public final boolean equals(final Object other) {
		if (other == this) {
			return true;
		}
		if (other instanceof DelElement) {
			DelElement otherElement = (DelElement) other;
			if (
					(
							(otherElement.getTarget() == null && this.getTarget() == null)
							|| otherElement.getTarget().equals(this.getTarget())
					) && 
					(
							otherElement.getAccompanyingDeletions().equals(this.getAccompanyingDeletions())
					)
				) {
				return true;
			}
		}
		return false;		
	}
}
