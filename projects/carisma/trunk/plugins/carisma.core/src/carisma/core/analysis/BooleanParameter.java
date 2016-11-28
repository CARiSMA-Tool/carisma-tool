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
 * Representation of a boolean check parameter.
 * @see CheckParameter
 * @author wenzel
 *
 */
public class BooleanParameter extends CheckParameter {
	/**
	 * 
	 * @param descriptor the CheckParameterDescriptor
	 */
	public BooleanParameter(final CheckParameterDescriptor descriptor) {
		super(descriptor, !descriptor.isOptional());
	}
	/**
	 * 
	 * @param descriptor the CheckParameterDescriptor
	 * @param value its value
	 */
	public BooleanParameter(final CheckParameterDescriptor descriptor, final boolean value) {
		super(descriptor, !descriptor.isOptional());
		this.value = value;
	}
	/**
	 * 
	 */
	private boolean value;
	/**
	 * getter for value.
	 * @return value of this parameter
	 */
	public final boolean getValue() {
		return this.value;
	}
	/**
	 * setter for value.
	 * @param value of this parameter
	 */
	public final void setValue(final boolean value) {
		this.value = value;
	}
	
}
