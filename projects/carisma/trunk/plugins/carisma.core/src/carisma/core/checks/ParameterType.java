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
package carisma.core.checks;

/**
 * Enumeration of different types of parameter that can be required by analysis plugins. 
 * @author wenzel
 *
 */
public enum ParameterType {

	STRING("String"),
	INTEGER("int"),
	FLOAT("float"),
	BOOLEAN("boolean"),
	INPUTFILE("InputFile"),
	OUTPUTFILE("OutputFile"),
	FOLDER("Folder");
	
	private String name;
	private ParameterType(String name) {
		this.name = name;
	}
	
	public static ParameterType byName(String name) {
		for (ParameterType t : values()) {
			if (t.name.equalsIgnoreCase(name)) {
				return t;
			}
		}
		throw new IllegalArgumentException("The given parameter is not a valid parameter type: "+name);
	}
	
}
