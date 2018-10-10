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
 * A delta element that describes the substitution
 * of the target element with a set of one or more
 * added elements.
 * TODO: The substituted element should probably have
 * - a set of elements that are deleted when the old
 * element is removed
 * - a set of elements that are kept when the old
 * element is removed
 * - some way to specify where the kept old elements
 * are placed in the newly added elements
 * - some way to specify where the old connections are
 * connected to the new elements 
 * @author Daniel Warzecha
 *
 */
public class SubstElement extends AdditiveElement {
	private List<EObject> accompanyingDeletions = null;
	
	private List<AddElement> components = null;
	
	public SubstElement(
			final EObject newTarget,
			final List<AddElement> theSubstitutes) {
		super(newTarget);
		this.components = new ArrayList<>();
		this.components.addAll(theSubstitutes);
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
			for (EObject newDeletion : newDeletions) {
				addDeletion(newDeletion);
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
		if (deletedElement != null && (!this.accompanyingDeletions.contains(deletedElement))) {
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
				addDeletion(newDeletion);
			}
		}
		return true;
	}

	public boolean removeDeletion(final EObject oldDeletion) {
		return this.accompanyingDeletions.remove(oldDeletion);
	}
	
	public List<AddElement> getComponents() {
		return this.components;
	}
	
	public List<AddElement> getAllAddedElements() {
		List<AddElement> allAddedElements = new ArrayList<>();
		for (AddElement addElem : this.components) {
			allAddedElements.addAll(addElem.getAllAddedElements());
		}
		return allAddedElements;
	}
	
	/**
	 * Replaces the components of the subst element with
	 * a new set of components.
	 * @param newComponents - new components
	 */
	public void replaceComponents(final List<AddElement> newComponents) {
		for (AddElement c : this.components) {
			c.setParent(null);
		}
		this.components.clear();
		for (AddElement c : newComponents) {
			addComponent(c);
		}
	}
	
	public boolean addComponent(final AddElement newComponent) {
		if (newComponent != null && (!this.components.contains(newComponent))) {
			newComponent.setParent(this);
			return this.components.add(newComponent);			
		}
		return false;
	}
	
	public boolean addComponents(final List<AddElement> newComponents) {
		for (AddElement de : newComponents) {
			addComponent(de);
		}
		return true;
	}

	public boolean removeComponent(final AddElement oldComponent) {
		if (this.components.contains(oldComponent)) {
			oldComponent.setParent(null);
			return this.components.remove(oldComponent);
		}
		return false;
	}
	
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append("Substituting "
				+ EObjectUtil.getTypeAndName(this.getTarget())
				+ " with ");
		buf.append(this.components.toString());
		return buf.toString();
	}
	
	@Override
	public final boolean equals(final Object other) {
		if (other == this) {
			return true;
		}
		if (other instanceof SubstElement) {
			SubstElement otherElement = (SubstElement) other;
			if (
					(
							(otherElement.getTarget() == null && this.getTarget() == null)
							|| otherElement.getTarget().equals(this.getTarget())
					) && 
					(
							otherElement.getAccompanyingDeletions().equals(this.getAccompanyingDeletions())
					) &&
					(
							otherElement.getComponents().equals(this.getComponents())
					)
				) {
				return true;
			}
		}
		return false;
	}
}
