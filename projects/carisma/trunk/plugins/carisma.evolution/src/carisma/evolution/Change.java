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
import java.util.List;
/**
 * A change consists of several alternatives,
 * of which only one can be
 * applied to the model.
 * Each change has a reference which is used in constraints.
 * Each change may have a constraint. The change may only be applied
 * if the constraint is fulfilled.
 * 
 * @author Daniel Warzecha
 *
 */
public class Change {
	/**
	 * A reference to use in constraints.
	 */
	private String ref;
	/**
	 * The constraints to be checked before application.
	 */
	private List<ChangeConstraint> constraints = null;
	
	private List<ChangeConstraint> constraintsInvolvedIn = null;
	/**
	 * The set of alternatives in the change.
	 */
	private List<Alternative> alternatives;
	/**
	 * Constructor for a Change with an empty constraint.
	 * The change must have a non-empty string as a reference.
	 * @param newRef - reference name of change
	 */
	public Change(final String newRef) {
		init();
		setRef(newRef);
	}
	/**
	 * Constructor for a Change with a non-empty FOL constraint.
	 * The change must have a non-empty string as a reference.
	 * @param newRef - reference name of change
	 * @param newConstraint - FOL constraint 
	 */
	public Change(final String newRef, final ChangeConstraint newConstraint) {
		init();
		setRef(newRef);
		addConstraint(newConstraint);
	}

	/**
	 * Constructor for a Change with a non-empty FOL constraint.
	 * The change must have a non-empty string as a reference.
	 * @param newRef - reference name of change
	 * @param newConstraint - List of FOL constraints
	 */
	public Change(final String newRef, final List<ChangeConstraint> newConstraints) {
		init();
		setRef(newRef);
		replaceConstraints(newConstraints);
	}
	
	private void init() {
		if (constraints == null) {
			constraints = new ArrayList<ChangeConstraint>();
		}
		constraints.clear();
		if (alternatives == null) {
			alternatives = new ArrayList<Alternative>();
		}
		alternatives.clear();
		if (constraintsInvolvedIn == null) {
			constraintsInvolvedIn = new ArrayList<ChangeConstraint>();
		}
		constraintsInvolvedIn.clear();
	}
	
	
	public final String getRef() {
		return ref;
	}
	/**
	 * Sets the reference name to a non-empty string.
	 * @param newRef - new reference name
	 */
	public void setRef(final String newRef) {
		if (newRef.equals("")) {
			// TODO: Was dann?
		} else {
			ref = newRef;
		}
	}

	public List<ChangeConstraint> getConstraints() {
		return Collections.unmodifiableList(constraints);
	}

	public final void addConstraint(final ChangeConstraint newConstraint) {
		if (newConstraint != null && !constraints.contains(newConstraint)) {
			constraints.add(newConstraint);
		}
	}

	public void addConstraints(final List<ChangeConstraint> newConstraints) {
		if (newConstraints != null) {
			for (ChangeConstraint addedConstraint : newConstraints) {
				addConstraint(addedConstraint);
			}
		}
	}

	public void removeConstraint(final ChangeConstraint oldConstraint) {
		constraints.remove(oldConstraint);
	}

	public final void replaceConstraints(final List<ChangeConstraint> newConstraints) {
		clearConstraints();
		addConstraints(newConstraints);
	}

	public void clearConstraints() {
		constraints.clear();
	}

	public List<Alternative> getAlternatives() {
		return Collections.unmodifiableList(alternatives);
	}
	/**
	 * Replaces all alternatives in the change with a new set of alternatives.
	 * @param newAlternatives - new set of alternatives
	 */
	public void replaceAlternatives(final List<Alternative> newAlternatives) {
		clearAlternatives();
		if (newAlternatives != null) {
			alternatives.addAll(newAlternatives);
		}
	}
	/**
	 * Adds a new alternative to the change.
	 * @param newAlternative - the new alternative
	 * @return - true, if the alternative could be added
	 */
	public void addAlternative(final Alternative newAlternative) {
		if (newAlternative != null) {
			alternatives.add(newAlternative);
		}
	}
	/**
	 * Removes an old alternative from the change.
	 * @param oldAlternative - the old alternative to remove
	 * @return - true, if alternative could be removed
	 */
	public void removeAlternative(final Alternative oldAlternative) {
		if (oldAlternative != null) {
			alternatives.remove(oldAlternative);
		}
	}
	/**
	 * Empties the set of alternatives.
	 */
	public void clearAlternatives() {
		alternatives.clear();
	}

}
