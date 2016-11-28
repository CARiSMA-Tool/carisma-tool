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
public class RegisterNotInUseException extends Exception {
	/**
	 * 
	 */
	private static final long serialVersionUID = -331580053429801961L;
	/**
	 * Name of this exception.
	 */
	private String name;
	/**
	 * Constructor.
	 * @param registerName Name of register
	 */
	public RegisterNotInUseException(final String registerName) {
		super("Register with name '" + registerName + "' is not in use!");
		this.name = registerName;
	}
	/**
	 * getter for name of register.
	 * @return the name
	 */
	public final String getName() {
		return this.name;
	}

}
