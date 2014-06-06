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
		metaclassName = ParserUtils.findMetaclassName(grammar);
		keyValuePairs = ParserUtils.findKeyValuePairs(grammar.replaceFirst(metaclassName, ""));
		contents = new ArrayList<SimpleElementDescription>();
		String contentsValue = keyValuePairs.get("contents");
		if (contentsValue != null) {
			List<String> extractedDescriptions = ParserUtils.extract(contentsValue.substring(1, contentsValue.length() - 1), ',');
			for (String extractedDescription : extractedDescriptions) {
				contents.add(new SimpleElementDescription(extractedDescription));
			}
			keyValuePairs.remove("contents");
		}
	}
	
	public String getMetaclassName() {
		return metaclassName;
	}
	
	public Map<String,String> getKeyValuePairs() {
		return keyValuePairs;
	}
	
	public List<SimpleElementDescription> getContainedElements() {
		return contents;
	}
	
	@Override
	public boolean equals(final Object other) {
		if (other == this) {
			return true;
		}
		if (other instanceof SimpleElementDescription) {
			SimpleElementDescription otherDescription = (SimpleElementDescription) other;
			if (otherDescription.getMetaclassName().equals(metaclassName)
					&& otherDescription.getKeyValuePairs().equals(keyValuePairs)
					&& otherDescription.getContainedElements().equals(contents)) {
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
	
	public boolean isValid() {
		if (getGrammarString() == null || getGrammarString().isEmpty()) {
			return false;
		}
		if (metaclassName == null) {
			return false;
		}
		if (keyValuePairs == null) {
			return false;
		}
		if (contents == null) {
			return false;
		}
		for (SimpleElementDescription sed : contents) {
			if (!sed.isValid()) {
				return false;
			}
		}
		return true;
	}
}
