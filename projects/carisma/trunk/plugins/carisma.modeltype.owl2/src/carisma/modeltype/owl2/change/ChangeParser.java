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

import java.util.HashMap;

public class ChangeParser {

	private static final String REGEX_PROPERTY = "(\\W|\\w)+";
	private static final String REGEX_ADD_ELEMENT_TAIL = "->\\w+\\("
			+ REGEX_PROPERTY
			+ "(," + REGEX_PROPERTY + ")*\\)";
	private static final String REGEX_ADD_ELEMENT = "\\w+(\\.\\w+)+" + REGEX_ADD_ELEMENT_TAIL;
	private static final String REGEX_DEL_ELEMENT = "\\w+(\\.\\w+)*\\.!\\w+(\\.\\w+)*=\""+ REGEX_PROPERTY + "\"";
	private static final String REGEX_SUBST_ELEMENT = REGEX_DEL_ELEMENT + REGEX_ADD_ELEMENT_TAIL;
	
	/*
	 * HashMap Content:
	 * -AxiomID
	 * -Navigation (dot-separated and trimmed)
	 * -Object
	 * -ObjectProperties (comma-separated and trimmed)
	 * 
	 */
	public static HashMap<String, String> parseAddElement(String encodedAddElement) {
		HashMap<String, String> result = new HashMap<String, String>();
		if (isAddElement(encodedAddElement)) {
			String[] splittedElement = encodedAddElement.split("->");
			String[] navigation = splittedElement[0].split("\\.");
			
			String navigationString = "";
			for (int i = 1; i < navigation.length - 1; i++) {
				navigationString += navigation[i] + ".";
			}
			navigationString += navigation[navigation.length - 1];

			String[] definedObject = splittedElement[1].split("\\(");
			String objectName = definedObject[0];
			String[] objectProperties = definedObject[1].substring(0, definedObject[1].length() - 1).split(",");
			String objectPropertiesTrimmed = "";
			
			for (int i = 0; i < objectProperties.length - 1; i++) {
				objectPropertiesTrimmed += objectProperties[i].trim() + ","; 
			}
			objectPropertiesTrimmed += objectProperties[objectProperties.length - 1].trim();
			
			
			result.put("axiomid", navigation[0]);
			result.put("navigation", navigationString);
			result.put("object", objectName);
			result.put("objectproperties", objectPropertiesTrimmed);
			
			/*System.out.println("AxiomID - " + navigation[0]);
			System.out.println("Navigation - " + navigationString);
			System.out.println("Object - " + objectName);
			System.out.println("ObjectProperties - " + objectPropertiesTrimmed);*/
		}
		return result;
	}
	
	/*
	 * HashMap Content:
	 * -AxiomID
	 * -Navigation (dot-separated and trimmed)
	 * -ObjectValue
	 * 
	 */
	public static HashMap<String, String> parseDelElement(String encodedDelElement) {
		HashMap<String, String> result = new HashMap<String, String>();
		if (isDelElement(encodedDelElement)) {
			String[] splittedElement = encodedDelElement.split("=");
			String[] navigation = splittedElement[0].split("\\.");
			
			String navigationString = "";
			for (int i = 1; i < navigation.length - 1; i++) {
				navigationString += navigation[i] + ".";
			}
			navigationString += navigation[navigation.length - 1];

			String objectValue = splittedElement[1].substring(1, splittedElement[1].length() - 1);
			
			result.put("axiomid", navigation[0]);
			result.put("navigation", navigationString);
			result.put("objectvalue", objectValue);
			
			/*System.out.println("AxiomID - " + navigation[0]);
			System.out.println("Navigation - " + navigationString);
			System.out.println("ObjectValue - " + objectValue);*/
		}
		return result;
	}
	
	/*
	 * HashMap Content:
	 * -AxiomID
	 * -Navigation (dot-separated and trimmed)
	 * -ObjectValue
	 * -Object
	 * -ObjectProperties (comma-separated and trimmed)
	 * 
	 */
	public static HashMap<String, String> parseSubstElement(String encodedSubstElement) {
		HashMap<String, String> result = new HashMap<String, String>();
		if (isSubstElement(encodedSubstElement)) {
			String[] splittedElement = encodedSubstElement.split("->");
			
			result = parseDelElement(splittedElement[0]);
			
			String[] definedObject = splittedElement[1].split("\\(");
			String objectName = definedObject[0];
			String[] objectProperties = definedObject[1].substring(0, definedObject[1].length() - 1).split(",");
			String objectPropertiesTrimmed = "";
			
			for (int i = 0; i < objectProperties.length - 1; i++) {
				objectPropertiesTrimmed += objectProperties[i].trim() + ","; 
			}
			objectPropertiesTrimmed += objectProperties[objectProperties.length - 1].trim();
			
			result.put("object", objectName);
			result.put("objectproperties", objectPropertiesTrimmed);
		}
		return result;
	}
	
	public static boolean isAddElement(String encodedAddElement) {
		return encodedAddElement.matches(REGEX_ADD_ELEMENT);
	}
	
	public static boolean isDelElement(String encodedDelElement) {
		return encodedDelElement.matches(REGEX_DEL_ELEMENT);
	}
	
	public static boolean isSubstElement(String encodedSubstElement) {
		return encodedSubstElement.matches(REGEX_SUBST_ELEMENT);
	}
	
}
