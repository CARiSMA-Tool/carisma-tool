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
 * An alternative consists of one or more delta elements
 * that are applied simultaneously to the model.
 * @author Daniel Warzecha
 *
 */
public class Alternative {
	/**
	 * The list of delta elements in this alternative.
	 */
	private List<DeltaElement> deltaElements;
	
	/**
	 * Constructor for an alternative.
	 */
	public Alternative() {
		this.deltaElements = new ArrayList<>();
	}
	/**
	 * Returns the non-modifiable list of delta elements.  
	 * @return - list of delta elements
	 */
	public List<DeltaElement> getDeltaElements() {
		return Collections.unmodifiableList(this.deltaElements);
	}
	
	/**
	 * Replaces all delta elements in this alternative with a new set of delta elements.
	 * @param newDeltaElements - the new delta elements
	 */
	public void replaceDeltaElements(final List<? extends DeltaElement> newDeltaElements) {
		this.deltaElements.clear();
		this.deltaElements.addAll(newDeltaElements);
	}
	/**
	 * Adds a delta element to the alternative.
	 * @param newDeltaElement - the new delta element
	 * @return - true, if the element could be added
	 */
	public boolean addDeltaElement(final DeltaElement newDeltaElement) {
		return this.deltaElements.add(newDeltaElement);
	}
	/**
	 * Adds a list of delta elements to the alternative.
	 * @param newDeltaElements - the new delta elements
	 * @return - true, if the elements could be added
	 */	
	public boolean addDeltaElements(final List<? extends DeltaElement> newDeltaElements) {
		return this.deltaElements.addAll(newDeltaElements);
	}
	/**
	 * Removes a delta element from the alternative.
	 * @param oldDeltaElement - the element to remove
	 * @return - true, if the element could be removed
	 */
	public boolean removeDeltaElement(final DeltaElement oldDeltaElement) {
		return this.deltaElements.remove(oldDeltaElement);
	}
	/**
	 * Empties the set of delta elements.
	 */
	public void clearDeltaElements() {
		this.deltaElements.clear();
	}
}