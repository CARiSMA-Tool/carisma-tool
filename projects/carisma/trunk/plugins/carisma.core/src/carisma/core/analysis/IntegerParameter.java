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
 * Representation of an integer check parameter.
 * @see CheckParameter
 * @author wenzel
 *
 */
public class IntegerParameter extends CheckParameter {
	/**
	 * value for this parameter.
	 */
//FIXME: Non-initialized integer value (can be anything)
	private int value;
	/**
	 * Constructor.
	 * @param descriptor the CheckParameterDescriptor
	 */
	public IntegerParameter(final CheckParameterDescriptor descriptor) {
		super(descriptor, !descriptor.isOptional());
	}
	/**
	 * Constructor.
	 * @param descriptor the CheckParameterDescriptor
	 * @param value value of this paramter
	 */
	public IntegerParameter(final CheckParameterDescriptor descriptor, final int value) {
		super(descriptor, !descriptor.isOptional());
		this.value = value;
	}
	/**
	 * getter for value.
	 * @return value of this parameter
	 */
	public final int getValue() {
		return this.value;
	}
	/**
	 * setter for value.
	 * @param value value of this parameter
	 */
	public final void setValue(final int value) {
		this.value = value;
	}
	
}
