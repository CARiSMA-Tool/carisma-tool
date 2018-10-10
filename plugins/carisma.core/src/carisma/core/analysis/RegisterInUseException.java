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
/**
 * 
 */
public class RegisterInUseException extends Exception {
	/**
	 * 
	 */
	private static final long serialVersionUID = 2865783878801116184L;
	/**
	 * name of this exception.
	 */
	private String name;
	/**
	 * 
	 * @param registerName Name of register
	 */
	public RegisterInUseException(final String registerName) {
		super("Register with name '" + registerName + "' is already in use!");
		this.name = registerName;
	}
	/**
	 * getter for name.
	 * @return the name of register
	 */
	public final String getName() {
		return this.name;
	}

}
