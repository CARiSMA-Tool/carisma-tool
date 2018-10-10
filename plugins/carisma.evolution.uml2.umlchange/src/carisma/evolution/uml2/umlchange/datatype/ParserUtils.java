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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class ParserUtils {
	
	public static final String R_METACLASS_NAME = "(?<!\\w)[\\w: ]+(?=\\()";
	
	public static final String R_KEYVALUE_PAIR = "(?<=[\\(,])[\\w ]+=[\\w ]+(?=[\\),])";
	
	public static final String R_VALUE = "(?<=[\\(,])\\w+(?=[\\),])";
	
	public static final String R_NAMESPACE = "(?<=@)[\\w: ]+(?!\\w)";
	
	private ParserUtils() {
		
	}
	
	/**
	 * Finds and stores the indices of the separators.
	 * @param grammar - the string to parse
	 * @param separator - separates grammar string parts
	 * @return - list of indices where separators are
	 */
	private static List<Integer> findSeparatorIndices(
			final String grammar, final char separator) {
		int bracketLevelCount = 0;
		ArrayList<Integer> separatorIndices = new ArrayList<>(); 
		for (int index = 0;
		index < grammar.length(); index++) {
			char ch = grammar.charAt(index);
			switch (ch) {
			case '(':
			case '{':
			case '[':
			case '<':
				bracketLevelCount++;
				break;
			case ')':
			case '}':
			case ']':
			case '>':
				bracketLevelCount--;
				break;
			default:
				if (ch == separator
						&& bracketLevelCount == 0) {
					separatorIndices.add(Integer.valueOf(index));
				}
			}
		}
		return separatorIndices;
	}
	/**
	 * Breaks a grammar string into its parts and returns those.
	 * This method works different than the inbuilt split() in so far
	 * that bracket levels inside the string are accounted for.
	 * Example: "name=someName,contents=<some(name=other,value=someValue)>"
	 * is split into "name=someName","contents=<some(name=other,value=someValue)>"
	 * when using separator ",". 
	 * @param grammarString - the string to parse
	 * @param separator - part separator
	 * @return - list of grammar parts
	 */
	public static List<String> extract(
			final String grammarString,
			final char separator) {
		String grammar = grammarString;
		List<Integer> separatorIndices =
			findSeparatorIndices(grammar, separator);
		ArrayList<String> extractedStrings = new ArrayList<>();
		int indexBegin = 0;
		for (int sepIndex : separatorIndices) {
			extractedStrings.add(grammar.substring(indexBegin, sepIndex));
			indexBegin = sepIndex + 1; 
		}
		extractedStrings.add(grammar.substring(indexBegin));
		return extractedStrings;
	}
	
	/**
	 * Finds and returns the metaclass
	 * in the given simple element description.
	 * @param grammar - string to parse
	 * @return - metaclass of the simple element
	 */
	public static String findMetaclassName(final String grammar) {
		Pattern pattern = Pattern.compile(R_METACLASS_NAME);
		Matcher matcher = pattern.matcher(grammar);
		if (matcher.find()) {
			return matcher.group();
		}
		return "";
	}
	/**
	 * Finds and returns the namespace name
	 * in the given namespace description.
	 * Example for a namespace description input:
	 * "@some::namespace(key=value)"
	 * @param grammar - the string to parse
	 * @return - namespace name
	 */
	public static String findNamespace(final String grammar) {
		Pattern pattern = Pattern.compile(R_NAMESPACE);
		Matcher matcher = pattern.matcher(grammar);
		if (matcher.find()) {
			return matcher.group();
		}
		return "";
	}
	/**
	 * Collects the key/value-pairs
	 * in the given simple element description.
	 * The input for this method must be "(KeyValuePairs)"
	 * @param grammar - the string to parse
	 * @return - map of key/value-pairs
	 */
	public static Map<String, String> findKeyValuePairs(final String grammar) {
		Map<String, String> keyValuePairs = new HashMap<>();
		String rest = grammar.replaceFirst("^\\(", "");
		rest = rest.replaceFirst("\\)$", "");
		List<String> keyValueStrings = new ArrayList<>();
		keyValueStrings.addAll(extract(rest, ','));
		for (String keyValuePair : keyValueStrings) {
			if (keyValuePair.contains("=")) {
				String [] parts = keyValuePair.split("=", 2);
				keyValuePairs.put(parts[0].trim(), parts[1].trim());
			}
		}
	return keyValuePairs;
	}
	
	public static String generateSimpleElementDescriptionString(final String metaclass, final Map<String, String> valueMap) {
		// TODO: move Code here
		return "";
	}

	public static String getMatchingValues(final String refValue, final List<String> valueList) {
		StringBuffer completeRef = new StringBuffer();
		if (refValue != null && valueList != null && !(valueList.isEmpty())) {
			for (String value : valueList) {
				if (value.startsWith(refValue + "=")) {
					completeRef.append(value.replaceFirst(refValue + "=", ""));
					completeRef.append(",");
				}
			}
			if (completeRef.length() > 0) {
				completeRef.deleteCharAt(completeRef.lastIndexOf(","));
			}
		}
		
		return completeRef.toString();
	}

}
