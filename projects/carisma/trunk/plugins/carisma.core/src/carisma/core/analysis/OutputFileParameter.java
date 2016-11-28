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

import java.io.File;

import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;


/**
 * @author lidiya.kaltchev
 *
 */
public class OutputFileParameter extends CheckParameter {

	/**
	 * value for this parameter.
	 */
	private File value;
	
	/**
	 * Cosntuctor.
	 * @param descriptor CheckParameterDescriptor
	 */
	public OutputFileParameter(final CheckParameterDescriptor descriptor) {
		super(descriptor, !descriptor.isOptional());
	}
	
	/**
	 * Constuctor.
	 * @param descriptor CheckParameterDescriptor
	 * @param value File value
	 */
	public OutputFileParameter(final CheckParameterDescriptor descriptor, final File value) {
		super(descriptor, !descriptor.isOptional());
		this.value = value;
	}
	
	/**
	 * Is value inserted in GUI valid.
	 * @return true iff a valid File with full Path is inserted in GUI.
	 * false else.
	 */  
	public final boolean isInsertedValueValid() {		
		if (this.value == null) {
			if (getDescriptor().isOptional()) {
				return true;
			}
			return false;
		} else if ("".equals(this.value.getPath().trim())) {
 			if (getDescriptor().isOptional()) {
				return true;
			}
			return false;
		} else if (this.value.getName().contains("?") 
				|| this.value.getName().contains("*") 
				|| this.value.getName().contains("\"")
				|| this.value.getName().contains(":")
				|| this.value.getName().contains("<")
				|| this.value.getName().contains(">")
				|| this.value.getName().contains("|")
			    || this.value.isDirectory()
				|| !this.value.getAbsolutePath().equals(this.value.getPath())
				|| (this.value.getParentFile() != null && !this.value.getParentFile().isDirectory())) {
			return false;
		}
		return true;
	}

	/**
	 * getter for value.
	 * @return File the value
	 */
	public final File getValue() {
		return this.value;
	}

	/**
	 * setter for value.
	 * @param value File value
	 */
	public final void setValue(final File value) {
		this.value = value;
	}
}
