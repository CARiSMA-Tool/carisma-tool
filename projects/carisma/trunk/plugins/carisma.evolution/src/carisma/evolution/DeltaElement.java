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

import org.eclipse.emf.ecore.EObject;
/**
 * A delta element describes an atomic change applied to the target object.
 * 
 * @author Daniel Warzecha
 *
 */
public abstract class DeltaElement {
	/**
	 * The element affected by the change.
	 */
	private EObject target;
	/**
	 * Constructor for a delta element directly contained
	 * in the model.
	 * @param newTarget - element affected by change
	 */
	public DeltaElement(
			final EObject newTarget) {
		this.target = newTarget;
	}
	
	public EObject getTarget() {
		return this.target;
	}
	
	public void setTarget(final EObject newTarget) {
		this.target = newTarget;
	}
	
}
