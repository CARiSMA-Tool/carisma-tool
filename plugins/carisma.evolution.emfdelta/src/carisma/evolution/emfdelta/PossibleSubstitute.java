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
package carisma.evolution.emfdelta;

import java.util.HashMap;
import java.util.Map;

import carisma.evolution.DeltaElement;


/** 
 * A subclass of AnalyzeSubstituteObject to keep the informations about possible substitute objects.
 * @author Johannes Kowald
 */
public class PossibleSubstitute {
	
	/**
	 * The corresponding DeltaElement.
	 */
	private DeltaElement deltaElement = null;
	
	/**
	 * The changed values of an EditElement.
	 */
	private Map<String, Object> refValues;
	
	/**
	 * The target of a DeltaElement in the changed diagram.
	 */
	private Object chaTarget;
	
	//########################################################################################
	/**
	 * The target of a DeltaElement in the changed diagram.
	 * @param deltaElement - the corresponding DeltaElement
	 */
	public PossibleSubstitute(final DeltaElement deltaElement) {
		this.deltaElement = deltaElement;
		this.refValues = new HashMap<String, Object>();
	}
	
	//########################################################################################
	/**
	 * This method adds a key/value pair to the list of changed values.
	 * @param newKey - the key
	 * @param newValue - the value
	 * @return indicates the success of the method
	 */
	public final boolean addRefKeyValuePair(final String newKey, final Object newValue) {
		if (!newKey.equals("") && newValue != null) {	
			refValues.put(newKey, newValue);
			return true;
		}
		return false;
	}
	
	//########################################################################################
	/**
	 * This method returns the mapping of changed values.
	 * @return Map<String, Object> - the map
	 */
	public final Map<String, Object> getRefValues() {
		return refValues;
	}
	
	//########################################################################################
	/**
	 * This method returns a value, which is mapped to the parameter key.
	 * @param key - the given key
	 * @return Object which is mapped to the given key
	 */
	public final Object getRefValue(final String key) {
		return refValues.get(key);
	}
	
	//########################################################################################
	/**
	 * This method sets the changed target.
	 * @param newTarget - the object which is the target
	 * @return indicates the success of the method
	 */
	public final boolean setChaTarget(final Object newTarget) {
		chaTarget = newTarget;
		return true;
	}
	
	//########################################################################################
	/**
	 * This method returns the changed target.
	 * @return Object - the object which is the target
	 */
	public final Object getChaTarget() {
		return this.chaTarget;
	}
	
	//########################################################################################
	/**
	 * This method returns the corresponding DeltaElement.
	 * @return DeltaElement - the corresponding DeltaElement
	 */
	public final DeltaElement getDeltaElement() {
		return this.deltaElement;
	}
	
	//########################################################################################
	/**
	 * This method checks, if the actual possible substitute is an edge.
	 * @return a boolean which indicates the success of the check
	 */
	public final boolean isLink() {
		if (refValues.containsKey("client") && refValues.containsKey("supplier")) {
			return true;
		}
		if (refValues.containsKey("source") && refValues.containsKey("target")) {
			return true;
		}
		return false;
	}
	
	//########################################################################################
	/**
	 * This method returns the content of the actual object
	 * @return a string which represents the content of the actual object
	 */
	@Override
	public final String toString(){
		String returnString = "RefValues: " + refValues + " | DeltaElement: " + deltaElement.toString();
		if (chaTarget != null) {
			returnString += " | ChaTarget: " + chaTarget.toString();
		}
		return returnString;
	}
}
