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
package carisma.modeltype.owl2.change;

public enum ElementType {

	ANNOTATION("Annotation"),
	CLASS_ASSERTION("ClassAssertion"),
	DATA_PROPERTY_ASSERTION("DataPropertyAssertion"),
	INDIVIDUAL("Individual"),
	INDIVIDUAL_DECLARATION("IndividualDeclaration"),
	OBJECT_PROPERTY_ASSERTION("ObjectPropertyAssertion");
	
	private String value;
	
	private ElementType(String value) {
		this.value = value;
	}
	
	public String getValue() {
		return this.value;
	}
	
}
