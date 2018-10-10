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

import java.util.List;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;

import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.EditElement;


/** 
 * A Subclass to analyze added Objects of the EmfDiff implementation of the IDeltaDescriptionGenerator.
 * @author Johannes Kowald
 */
public class AnalyzeEditObject {
	
	/**
	 * A Change List of differences.
	 */
	private List<Change> deltaList;
	
	/**
	 * The AnalyzeSubstituteObject which contains ReferenceModel informations about possible Substitutes.
	 */
	private EMFDelta emfDelta;
	
	//########################################################################################
	/**
	 * The constructor of the AnalyseAddedObject class.
	 * @param newdeltaList - the current deltaList where the changes, alternatives and deltaElements are added
	 * @param newanalyzeSubstituteObject - the current AnalyzeSubstituteObject which will contain ReferenceModel informations about possible substitutes
	 */
	public AnalyzeEditObject(final List<Change> newdeltaList, final EMFDelta emfDelta) {
		this.deltaList = newdeltaList;
		this.emfDelta = emfDelta;
	}
	
	//########################################################################################
	/**
	 * The constructor of the AnalyseAddedObject class.
	 * @return deltaList - the current deltaList where the changes, alternatives and deltaElements are added
	 */
	public final List<Change> getDeltaList() {
		return this.deltaList;
	}
	
	//########################################################################################
	/**
	 * Analyzes a single EObject provided by the EMF Compare DiffModel with an EObject from the reference model as parent.
	 * @param chaElement - the element which is analyzed
	 * @param refElement - the element of the second diagram. Needed to compare the EStructuralFeatures, EAllAttributes and EReferences
	 * @param refID - a string which contains the refID for the new Change Object
	 */
	public final void analyzeEObject(final EObject chaElement, final EObject refElement, final String refID) {		
		EObject 		newTarget 				= refElement;
		newTarget								= EMFDeltaUMLHelper.convertToStereotypeIfSuitable(newTarget);
		EditElement 	newEditElement			= new EditElement(newTarget);
		
		if (!EMFDeltaUMLHelper.isStereotype(newTarget)) {
			analyzeEObjectDetails(newEditElement, refElement, chaElement);
		} else {
			// Only for UML
			EMFDeltaUMLHelper.analyzeEditedTaggedValues(newEditElement, refElement, chaElement);
		}
		
		Change newChange 			= new Change(refID);
		Alternative newAlternative 	= new Alternative();
		
		newChange.addAlternative(newAlternative);
		this.deltaList.add(newChange);
		newAlternative.addDeltaElement(newEditElement);
	}
		
	//########################################################################################
	/**
	 * Analyzes the EStructuralFeatures, EAttributes and EReferences of an EObject deeper.
	 * @param newEditElement - the EditElement which is filled with the found informations
	 * @param chaElement - the element which will be analyzed
	 * @param refElement - the element of the second diagram. Needed to compare the EStructuralFeatures, EAllAttributes and EReferences
	 */
	private void analyzeEObjectDetails(final EditElement newEditElement, 
			final EObject refElement, final EObject chaElement) {
		EClass elementClass = refElement.eClass();
			
		PossibleSubstitute possSubst = emfDelta.addPossibleSubstitute(newEditElement);
				
		for (EAttribute eA : elementClass.getEAllAttributes()) {
			if (!eA.isDerived()) {
				Object chaValue 	= chaElement.eGet(eA);
				Object refValue 	= refElement.eGet(eA);
				if (refValue != null && chaValue != null) {
					if (!refValue.equals(chaValue)) {
						possSubst.addRefKeyValuePair(eA.getName(), refValue);
						newEditElement.addKeyValuePair(eA.getName(), chaValue);
					}
				} else {
					newEditElement.addKeyValuePair(eA.getName(), chaValue);
				}
			}
		}
		
		for (EReference eR : elementClass.getEAllReferences()) {
			if (!eR.isDerived()) {
				Object chaValue 	= chaElement.eGet(eR);
				Object refValue		= refElement.eGet(eR);
				if (refValue != null) {
					if (!refValue.equals(chaValue)) {
						possSubst.addRefKeyValuePair(eR.getName(), refValue);
						if (chaValue != null) {
							newEditElement.addKeyValuePair(eR.getName(), chaValue);	
						}
					}
				} else {
					if (chaValue != null) {
						newEditElement.addKeyValuePair(eR.getName(),chaValue);
					}
				}
			}

		}
		
		if (!possSubst.isLink()) {
			emfDelta.removePossibleSubstitute(possSubst);
		}
	}
}
