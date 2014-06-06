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
package carisma.modeltype.owl2.type;

/**
 * Lists all available XML data types.
 * 
 * @author Marcel Michel
 */
public enum OWL2XMLPrimitiveType {

	STRING("#string"),
	BOOLEAN("#boolean"),
	DECIMAL("#decimal"),
	DOUBLE("#double");
	
	
	private String xmlString;
	private final String base = "http://www.w3.org/2001/XMLSchema";
	
	private OWL2XMLPrimitiveType(String xmlString) {
		this.xmlString = xmlString;
	}
	
	public String getValue() {
		return base + xmlString;
	}
}
