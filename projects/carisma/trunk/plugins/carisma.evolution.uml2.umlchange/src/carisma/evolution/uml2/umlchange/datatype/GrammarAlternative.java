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

import java.util.ArrayList;
import java.util.List;
/**
 * A UMLchange grammar change consists of
 * alternatives. These have one or more element
 * descriptions.
 * @author Daniel Warzecha
 *
 */
public class GrammarAlternative {
	/**
	 * The corresponding grammar string.
	 */
	private String grammarString = ""; // {Alternativenelemente}
	
	/**
	 * The element descriptions.
	 */
	private List<ElementDescription> descriptions = null;
	/**
	 * Creates an alternative with a grammar string.
	 * Collects the element descriptions of this alternative.
	 * @param grammar - string to parse
	 */
	public GrammarAlternative(final String grammar) {
		this.grammarString = grammar.trim();
		this.descriptions = new ArrayList<>();
		List<String> extractedDescriptions =
			ParserUtils.extract(
					this.grammarString.substring(1, this.grammarString.length() - 1), ',');
		for (String extractedDescription : extractedDescriptions) {
			if (extractedDescription.contains("=") || extractedDescription.endsWith("()")) {
				this.descriptions.add(new SimpleElementDescription(extractedDescription));
			} else if (extractedDescription.contains("@")) {
				this.descriptions.add(new NamespaceDescription(extractedDescription));
			}
		}
	}
	
	public String getGrammarString() {
		return this.grammarString;
	}
	
	public List<ElementDescription> getDescriptions() {
		return this.descriptions;
	}
	
	public boolean hasNamespaceDescription() {
		for (ElementDescription desc : this.descriptions) {
			if (desc instanceof NamespaceDescription) {
				return true;
			}
		}
		return false;
	}
	
	public boolean isValid() {
		if (this.grammarString == null || this.grammarString.isEmpty()) {
			return false;
		}
		if (this.descriptions == null || this.descriptions.isEmpty()) {
			return false;
		}
		for (ElementDescription desc : this.descriptions) {
			if (!desc.isValid()) {
				return false;
			}
		}
		return true;
	}
}
