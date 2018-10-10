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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;


public class SubstFinder_CorrespondingLinks implements AnalyzeSubstituteObject {
	
	/**
	 * An List of possible Substitutes, which will be checked by searchForSubstitutes(..).
	 */
	private List<PossibleSubstitute> possSubstList;
	
	/**
	 * The List of Changes from the DeltaModel.
	 */
	private List<Change> deltaList;

	/**
	 * Constant value for the key "client".
	 */
	private static final String CLIENT = "client";
	
	/**
	 * Constant value for the key "supplier".
	 */
	private static final String SUPPLIER = "supplier";
	
	/**
	 * Constant value for the key "source".
	 */
	private static final String SOURCE = "source";
	
	/**
	 * Constant value for the key "target".
	 */
	private static final String TARGET = "target";
	
	
	/**
	 * A Mapping which saves the Substitutes, that the algorithm found.
	 */
	private Map<Object, Object> foundSubst;
	
	//########################################################################################
	/**
	 * The constructor of the AnalyzeSubstituteObject class.
	 * @param newdeltaList - the current deltaList where the found substitutes will be added
	 */
	public SubstFinder_CorrespondingLinks(final List<Change> deltaList, final List<PossibleSubstitute> possSubstList) {
		this.possSubstList = possSubstList;
		this.deltaList = deltaList;
		this.foundSubst = new HashMap<Object, Object>();
	}
	
	//########################################################################################
	/**
	 * This method analyzes the possible substitution list and creates, if possible, a new SubstElement for the DeltaModel.
	 * @param refID - the ReferenceID for the possible new SubstElement object
	 * @return a boolean which indicates the success of the method
	 */
	@Override
	public final boolean searchForSubstitutes(final int refID) {
		int localRefID = refID;
		for (PossibleSubstitute pS : possSubstList) {
			if (pS.getDeltaElement() instanceof EditElement) {
				EditElement editE = (EditElement) pS.getDeltaElement();
				
				Object refObject1 = null;
				Object refObject2 = null;
				Object chaObject1 = null;
				Object chaObject2 = null;
				AddElement addElement1 = null;
				AddElement addElement2 = null;
				
				// client/supplier
				if (editE.getValues().containsKey(CLIENT)) {
					chaObject1 = editE.getValues().get(CLIENT);
					if (chaObject1 instanceof EList<?>) {
						@SuppressWarnings("unchecked")
						EList<EObject> objList = (EList<EObject>) chaObject1;
						chaObject1 = objList.get(0);
					}
					chaObject2 = editE.getValues().get(SUPPLIER);
					if (chaObject2 instanceof EList<?>) {
						@SuppressWarnings("unchecked")
						EList<EObject> objList = (EList<EObject>) chaObject2;
						chaObject2 = objList.get(0);
					}
					
					refObject1 = pS.getRefValue(CLIENT);
					if (refObject1 instanceof EList<?>) {
						@SuppressWarnings("unchecked")
						EList<EObject> objList = (EList<EObject>) refObject1;
						refObject1 = objList.get(0);
					}
					refObject2 = pS.getRefValue(SUPPLIER);
					if (refObject2 instanceof EList<?>) {
						@SuppressWarnings("unchecked")
						EList<EObject> objList = (EList<EObject>) refObject2;
						refObject2 = objList.get(0);
					}
					
					addElement1 = searchForAdd(chaObject1);
					if (addElement1 != null) {
						editE.editKeyValuePair(CLIENT, addElement1);
					}
					
					addElement2 = searchForAdd(chaObject2);
					if (addElement2 != null) {
						editE.editKeyValuePair(SUPPLIER, addElement2);
					}
				}
				
				// source/target
				if (editE.getValues().containsKey(SOURCE)) {
					chaObject1 = editE.getValues().get(SOURCE);
					if (chaObject1 instanceof EList<?>) {
						@SuppressWarnings("unchecked")
						EList<EObject> objList = (EList<EObject>) chaObject1;
						chaObject1 = objList.get(0);
					}
					chaObject2 = editE.getValues().get(TARGET);
					if (chaObject2 instanceof EList<?>) {
						@SuppressWarnings("unchecked")
						EList<EObject> objList = (EList<EObject>) chaObject2;
						chaObject2 = objList.get(0);
					}
					
					refObject1 = pS.getRefValue(SOURCE);
					if (refObject1 instanceof EList<?>) {
						@SuppressWarnings("unchecked")
						EList<EObject> objList = (EList<EObject>) refObject1;
						refObject1 = objList.get(0);
					}
					refObject2 = pS.getRefValue(TARGET);
					if (refObject2 instanceof EList<?>) {
						@SuppressWarnings("unchecked")
						EList<EObject> objList = (EList<EObject>) refObject2;
						refObject2 = objList.get(0);
					}
										
					addElement1 = searchForAdd(chaObject1);
					if (addElement1 != null) {
						editE.editKeyValuePair(SOURCE, addElement1);
					}
					
					addElement2 = searchForAdd(chaObject2);
					if (addElement2 != null) {
						editE.editKeyValuePair(TARGET, addElement2);
					}
				}
				
				if (foundSubst.get(refObject1) == null || !foundSubst.get(refObject1).equals(chaObject1)) {					
					DelElement targetDel = searchForDelete(refObject1);

					if (addElement1 != null && targetDel != null && checkCorrespondingLinks(refObject1, addElement1)) {
						SubstElement newSubstElement = new SubstElement((EObject) refObject1, new ArrayList<AddElement>());
						if (addElement1.getAllAddedElements() != null) {
							newSubstElement.addComponents(addElement1.getAllAddedElements());
						}
						if (targetDel.getAccompanyingDeletions() != null) {
							newSubstElement.addDeletions(targetDel.getAccompanyingDeletions());
						}
												
						localRefID++;
						
						Change change = new Change(Integer.toString(localRefID));
						Alternative alternative = new Alternative();
						change.addAlternative(alternative);
						alternative.addDeltaElement(newSubstElement);
						
						deltaList.add(change);
						foundSubst.put(refObject1, chaObject1);
					}
				}
				
				if (foundSubst.get(refObject2) == null || !foundSubst.get(refObject2).equals(chaObject2)) {
					DelElement targetDel = searchForDelete(refObject2);

					if (addElement2 != null && targetDel != null && checkCorrespondingLinks(refObject2, addElement2)) {
						SubstElement newSubstElement = new SubstElement((EObject) refObject2, new ArrayList<AddElement>());
						if (addElement2.getAllAddedElements() != null) {
							newSubstElement.addComponents(addElement2.getAllAddedElements());
						}
						if (targetDel.getAccompanyingDeletions() != null) {
							newSubstElement.addDeletions(targetDel.getAccompanyingDeletions());
						}
						
						localRefID++;
						
						Change change = new Change(Integer.toString(localRefID));
						Alternative alternative = new Alternative();
						change.addAlternative(alternative);
						alternative.addDeltaElement(newSubstElement);
						
						deltaList.add(change);
						foundSubst.put(refObject2, chaObject2);
					}
				}
			}
		}
		return true;
	}
	
	//########################################################################################
	/**
	 * This method searches for an AddElement in the possible substitution list which fits to the parameter.
	 * @param object - The object, which has to be equal to the found AddElement
	 * @return The equal AddElement or null
	 */
	private AddElement searchForAdd(final Object object) {
		for (PossibleSubstitute pS : possSubstList) {
			DeltaElement dE = pS.getDeltaElement();
			if (dE instanceof AddElement
					&& pS.getChaTarget().equals(object)) {
				return (AddElement) dE;
			}
		}
		return null;
	}
	
	//########################################################################################
	/**
	 * This method searches for an DelElement in the deltaList which fits to the parameter.
	 * @param object - The object, which has to be equal to the found DelElement
	 * @return The equal DelElement or null
	 */
	private DelElement searchForDelete(final Object object) {
		Object actObj = listToObject(object);
		for (Change change : deltaList) {
			for (Alternative alternative : change.getAlternatives()) {
				for (DeltaElement deltaElement : alternative.getDeltaElements()) {
					if (deltaElement instanceof DelElement) {
						DelElement delElement = (DelElement) deltaElement;
						if (delElement.getTarget().equals(actObj)) {
							return delElement;
						}
					}
				}
			}
		}
		return null;
	}
	
	//########################################################################################
	/**
	 * This method searches for an DelElement in the deltaList which fits to the parameter.
	 * @param object - The object, which is maybe a list of objects
	 * @return the single object
	*/
	private Object listToObject(final Object object) {
		if (object instanceof EList<?>) {
			@SuppressWarnings("unchecked")
			EList<EObject> objList = (EList<EObject>) object;
			return objList.get(0);
		}
		return object;
	}
	
	//########################################################################################
	/**
	 * If all edges, corresponding to parameter refObject, are edited to parameter chaObject in the same way, this method return true.
	 * @param refObject - The reference object
	 * @param chaObject - The object from the changed diagram
	 * @return If all corresponding edges are edited in the same way, the method returns true, else false
	 */
	private boolean checkCorrespondingLinks(final Object refObject, final AddElement addelement) {
		for (PossibleSubstitute pS : possSubstList) {
			if (pS.getDeltaElement() instanceof EditElement) {
				EditElement editE = (EditElement) pS.getDeltaElement();
				// client/supplier
				if (pS.getRefValues().containsKey(CLIENT)) {					
					// If the deleted Object was the client, but the added Object isn´t the client now
					if ((pS.getRefValue(CLIENT).equals(refObject))
							&& !(editE.getValues().get(CLIENT).equals(addelement))) {
						return false;
					}
					// If the deleted Object was the supplier, but the added Object isn´t the supplier now
					if ((pS.getRefValue(SUPPLIER).equals(refObject))
							&& !(editE.getValues().get(SUPPLIER).equals(addelement))) {
						return false;
					}
				}
				
				// source/target
				if (pS.getRefValues().containsKey(SOURCE)) {										
					// If the deleted Object was the source, but the added Object isn´t the source now
					if ((pS.getRefValue(SOURCE).equals(refObject))
							&& !(editE.getValues().get(SOURCE).equals(addelement))) {
						return false;
					}
					// If the deleted Object was the target, but the added Object isn´t the target now
					if ((pS.getRefValue(TARGET).equals(refObject))
							&& !(editE.getValues().get(TARGET).equals(addelement))) {
						return false;
					}
				}
			}
		}
		return true;
	}

}
