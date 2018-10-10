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
import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;

import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.ChangeConstraint;
import carisma.evolution.ConstraintType;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;

/** 
 * A Subclass to analyze delete Objects of the EmfDiff implementation of the IDeltaDescriptionGenerator.
 * @author Johannes Kowald
 */
public class AnalyzeDeletedObject {
	
	/**
	 * A Change ArrayList of differences.
	 */
	private List<Change> deltaList;
		
	//########################################################################################
	/**
	 * The constructor of the AnalyseAddedObject class.
	 * @param newdeltaList - the current deltaList where the changes, alternatives and deltaElements are added
	 */
	public AnalyzeDeletedObject(final List<Change> newdeltaList) {
		this.deltaList = newdeltaList;
	}
	
	//########################################################################################
	/**
	 * Analyzes a single EObject provided by the EMF Compare DiffModel with an AddElement as parent.
	 * @param element - the element which is analyzed
	 * @param refID - a string which contains the refID for the new Change Object
	 * @param constraint - a string which contains a constraint for the new Change Object
	 * @return returns the accompanying deletions to fill up the parent elements recursively
	 */
	public final Change analyzeEObject(final EObject element, final String refID, final Change requiredChange) {
		EObject 	newTarget 				= EMFDeltaUMLHelper.convertToStereotypeApplicationIfSuitable(element);
		DelElement 	newDelElement			= new DelElement(newTarget);
		
		Change newChange = new Change(refID);
		if (requiredChange != null) { 
			newChange.addConstraint(new ChangeConstraint(ConstraintType.REQ, requiredChange, newChange));
		}
		Alternative newAlternative = new Alternative();
		newChange.addAlternative(newAlternative);
		this.deltaList.add(newChange);
		newAlternative.addDeltaElement(newDelElement);

		if (!EMFDeltaUMLHelper.isStereotypeApplication(newTarget)) {
			List<ChangeConstraint> additionalConstraints = checkForSubEObjects(element, refID, newDelElement, newChange);
			newChange.addConstraints(additionalConstraints);
			if (EMFDeltaHelper.getAnalysisMode() == EMFDeltaHelper.ANALYSIS_MODE_UML) {
				// Only for UML models
				List<ChangeConstraint> addConstraintsStereoTagValues = 
						EMFDeltaUMLHelper.checkForDeletedStereotypes(
								element, 
								refID, 
								newDelElement, 
								newChange,
								this);
				newChange.addConstraints(addConstraintsStereoTagValues);
			}
		} else {
			// Only for UML models
			List<EObject> accompanyingDeletions = new ArrayList<EObject>();
			List<ChangeConstraint> addConstraintsTagValues = 
					EMFDeltaUMLHelper.checkForDeletedTaggedValues(
							(carisma.modeltype.uml2.StereotypeApplication) newTarget, 
							accompanyingDeletions, 
							newChange, 
							this);
			newChange.addConstraints(addConstraintsTagValues);
			newDelElement.addDeletions(accompanyingDeletions);
		}
		return newChange;
	}
		
	//########################################################################################
	/**
	 * Analyzes the EReferences for SubElements which have to be analyzed.
	 * @param element - the element which will be analyzed
	 * @param refID - a string which contains the refID for the new Change Object
	 * @param newDelElement - the actual DeltaModel DelElement
	 * @return List<ChangeConstraint> - the List of constraints for the parent Element
	 */
	private List<ChangeConstraint> checkForSubEObjects(final EObject element, final String refID, final DelElement newDelElement, final Change newChange) {
		EClass elementClass = element.eClass();
		ArrayList<ChangeConstraint> returnConstraintList = new ArrayList<ChangeConstraint>();
		List<EObject> accompayingDeletions = new ArrayList<EObject>();
		Change innerChange = null;
		
		// Iterate over all eReferences to find SubElements
		for (EReference eR : elementClass.getEReferences()) {
			if ((!eR.isDerived()) 
					&& eR.isContainment() 
					&& eR.isMany()) {
				@SuppressWarnings("unchecked")
				EList<EObject> valueList = (EList<EObject>) element.eGet(eR);
				int refIDsub = 0;
				// If it is a list of values
				if (valueList != null) {
					for (EObject innerElement : valueList) {
						refIDsub++;
						newDelElement.addDeletion(innerElement);
						innerChange = analyzeEObject(innerElement, refID + "-" + Integer.toString(refIDsub), null);
						addAllInnerDeletions(accompayingDeletions, innerChange);
						newDelElement.addDeletions(accompayingDeletions);
						ChangeConstraint newChangeConstraint = new ChangeConstraint(ConstraintType.REQ,innerChange,newChange);
						returnConstraintList.add(newChangeConstraint);
					}
				// If it is a single value	
				} else {
					EObject innerElement = (EObject) element.eGet(eR);
					if (innerElement != null) {
						newDelElement.addDeletion(innerElement);
						innerChange = analyzeEObject(innerElement, refID + "-1", null);
						addAllInnerDeletions(accompayingDeletions, innerChange);
						newDelElement.addDeletions(accompayingDeletions);
						ChangeConstraint newChangeConstraint = new ChangeConstraint(ConstraintType.REQ,innerChange,newChange);
						returnConstraintList.add(newChangeConstraint);
					}
				}
			}
		}		
		return returnConstraintList;
	}
	
	//########################################################################################
	/**
	 * Adds all deletions belonging to the given Change object to the given accompanyingDeletions list.
	 * @param accompayingDeletions The list of accompanying deletions.
	 * @param change A Change object which holds one or more deletions.
	 */
	protected void addAllInnerDeletions(List<EObject> accompanyingDeletions, Change change) {
		for (DeltaElement de : change.getAlternatives().get(0).getDeltaElements()) {
			if (de instanceof DelElement && de.getTarget() != null) { 
				accompanyingDeletions.add(de.getTarget());
			}
		}
	}
}
