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
import java.util.Map;

/**
 * A simple element description in the UMLchange grammar.
 * An instance consists of the name of a metaclass,
 * some key/value pairs describing property values
 * and further SimpleElementDescriptions contained in this
 * element.  
 * @author Daniel Warzecha
 *
 */
public class SimpleElementDescription extends ElementDescription {
	/**
	 * Metaclass name of the element.
	 */
	private String metaclassName = "";
	
	/**
	 * Constant int value for hashCode computation.
	 */
	private static final int BASIS = 17;
	
	/**
	 * Constant int value for hashCode computation.
	 */
	private static final int FACTOR = 37;
	/**
	 * Property values of the element.
	 */
	private Map<String, String> keyValuePairs = null;
	/**
	 * Contained elements of the element.
	 */
	private List<SimpleElementDescription> contents = null;
	/**
	 * Constructor using a UMLchange grammar string containing
	 * a simple element description. 
	 * @param grammar - grammar string to parse
	 */
	public SimpleElementDescription(final String grammar) {
		super(grammar);
		this.metaclassName = ParserUtils.findMetaclassName(grammar);
		this.keyValuePairs = ParserUtils.findKeyValuePairs(grammar.replaceFirst(this.metaclassName, ""));
		this.contents = new ArrayList<>();
		String contentsValue = this.keyValuePairs.get("contents");
		if (contentsValue != null) {
			List<String> extractedDescriptions = ParserUtils.extract(contentsValue.substring(1, contentsValue.length() - 1), ',');
			for (String extractedDescription : extractedDescriptions) {
				this.contents.add(new SimpleElementDescription(extractedDescription));
			}
			this.keyValuePairs.remove("contents");
		}
	}
	
	public String getMetaclassName() {
		return this.metaclassName;
	}
	
	public Map<String,String> getKeyValuePairs() {
		return this.keyValuePairs;
	}
	
	public List<SimpleElementDescription> getContainedElements() {
		return this.contents;
	}
	
	@Override
	public boolean equals(final Object other) {
		if (other == this) {
			return true;
		}
		if (other instanceof SimpleElementDescription) {
			SimpleElementDescription otherDescription = (SimpleElementDescription) other;
			if (otherDescription.getMetaclassName().equals(this.metaclassName)
					&& otherDescription.getKeyValuePairs().equals(this.keyValuePairs)
					&& otherDescription.getContainedElements().equals(this.contents)) {
				return true;
			}			
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		
		int result = BASIS;
		result = FACTOR * result + ((this.metaclassName == null) ? 0 : this.metaclassName.hashCode());
		result = FACTOR * result + ((this.keyValuePairs == null) ? 0 : this.keyValuePairs.hashCode());
		result = FACTOR * result + ((this.contents == null) ? 0 : this.contents.hashCode());

		return result;
	}
	
	@Override
	public boolean isValid() {
		if (getGrammarString() == null || getGrammarString().isEmpty()) {
			return false;
		}
		if (this.metaclassName == null) {
			return false;
		}
		if (this.keyValuePairs == null) {
			return false;
		}
		if (this.contents == null) {
			return false;
		}
		for (SimpleElementDescription sed : this.contents) {
			if (!sed.isValid()) {
				return false;
			}
		}
		return true;
	}
}
