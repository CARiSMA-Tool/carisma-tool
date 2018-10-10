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
 * A class to encapsulate UMLchange grammar strings.
 * @author Daniel Warzecha
 *
 */
public class GrammarBlock {
	/**
	 * The original String function the GrammarBlock
	 * is initialized with.
	 */
	private String grammarString = null;
	
	/**
	 * The alternatives.
	 */
	private List<GrammarAlternative> alternatives = null;

	public GrammarBlock(final String grammar) {
		this.grammarString = grammar.trim();
		this.alternatives = new ArrayList<>();
		List<String> extractedAlternatives =
			ParserUtils.extract(
					this.grammarString, ',');
		for (String extractedAlternative : extractedAlternatives) {
			this.alternatives.add(new GrammarAlternative(extractedAlternative));
		}			
	}
	
	public String getGrammarString() {
		return this.grammarString;
	}
	
	public List<GrammarAlternative> getAlternatives() {
		return this.alternatives;
	}
	
	public boolean hasNamespaceDescriptions() {
		for (GrammarAlternative alt : this.alternatives) {
			if (alt.hasNamespaceDescription()) {
				return true;
			}
		}
		return false;
	}
	
	public boolean isValid() {
		if (this.alternatives == null || this.alternatives.isEmpty()) {
			return false;
		}
		if (this.grammarString == null || this.grammarString.isEmpty()) {
			return false;
		}
		for (GrammarAlternative alt : this.alternatives) {
			if (!alt.isValid()) {
				return false;
			}
			
		}
		return true;
	}
}
