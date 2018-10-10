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
package carisma.core.analysis;

import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;


/**
 * Representation of an float check parameter.
 * @see CheckParameter
 * @author wenzel
 *
 */
public class FloatParameter extends CheckParameter {
	/**
	 * value.
	 */
	//FIXME: Non-initialized float value (can be anything)
	private float value;
	/**
	 * Constructor.
	 * @param descriptor the CheckParameterDescriptor
	 */
	public FloatParameter(final CheckParameterDescriptor descriptor) {
		super(descriptor, !descriptor.isOptional());
	}
	/**
	 * Constructor.
	 * @param descriptor the CheckParameterDescriptor
	 * @param value value of this parameter
	 */
	public FloatParameter(final CheckParameterDescriptor descriptor, final float value) {
		super(descriptor, !descriptor.isOptional());
		this.value = value;
	}
	/**
	 * getter for value.
	 * @return the value in float
	 */
	public final float getValue() {
		return this.value;
	}
	/**
	 * setter for value.
	 * @param value the value in float
	 */
	public final void setValue(final float value) {
		this.value = value;
	}
	
}
