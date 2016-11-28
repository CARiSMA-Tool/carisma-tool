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
package carisma.evolution.uml2.umlchange.datatype;

/**
 * Simple encapsulation of a namespace description.
 * @author Daniel Warzecha
 *
 */
public class NamespaceDescription extends ElementDescription {
	/**
	 * Name of the referenced namespace.
	 */
	private String namespaceName = "";
	
	public String getNamespaceName() {
		return this.namespaceName;
	}
	
	public NamespaceDescription(final String grammar) {
		super(grammar);
		this.namespaceName = ParserUtils.findNamespace(grammar);
	}
	
	@Override
	public boolean isValid() {
		if (getGrammarString() == null || getGrammarString().isEmpty()) {
			return false;
		}
		if (this.namespaceName == null || this.namespaceName.isEmpty()) {
			return false;
		}
		return true;
	}
}
