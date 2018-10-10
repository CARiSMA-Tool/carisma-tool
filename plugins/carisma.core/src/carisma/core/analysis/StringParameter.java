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
 * Representation of a string check parameter.
 * @see CheckParameter
 * @author wenzel
 *
 */
public class StringParameter extends CheckParameter {
	/**
	 * value of this parameter.
	 */
	private String value;
	/**
	 * Constructor.
	 * @param descriptor CheckParameterDescriptor
	 */
	public StringParameter(final CheckParameterDescriptor descriptor) {
		super(descriptor, !descriptor.isOptional());
	}
	/**
	 * Constructor.
	 * @param descriptor CheckParameterDescriptor
	 * @param value value of this parameter
	 */
	public StringParameter(final CheckParameterDescriptor descriptor, final String value) {
		super(descriptor, !descriptor.isOptional());
		this.value = value;
	}
	/**
	 * getter for value.
	 * @return value of this parameter
	 */
	public final String getValue() {
		return this.value;
	}
	/**
	 * setter for value.
	 * @param value value of this parameter
	 */
	public final void setValue(final String value) {
		this.value = value;
	}
	
}
