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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;

import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.ChangeConstraint;
import carisma.evolution.ConstraintType;

/** 
 * A Subclass to analyze added Objects of the EmfDiff implementation of the IDeltaDescriptionGenerator.
 * @author Johannes Kowald
 */
public class AnalyzeAddedObject {
	
	/**
	 * A Change ArrayList of differences.
	 */
	private List<Change> deltaList;
	
	/**
	 * The AnalyzeSubstituteObject which contains ReferenceModel informations about possible Substitutes.
	 */
	private EMFDelta emfDelta;
		
	//########################################################################################
	/**
	 * The constructor of the AnalyseAddedObject class.
	 * @param newdeltaList The current deltaList where the changes, alternatives and deltaElements are added
	 * @param newanalyzeSubstituteObject - the current AnalyzeSubstituteObject which will contain ReferenceModel informations about possible substitutes
	 */
	public AnalyzeAddedObject(final List<Change> deltaList, final EMFDelta emfDelta) {
		this.deltaList = deltaList;
		this.emfDelta = emfDelta;
	}
		
	//########################################################################################
	/**
	 * Analyzes a single EObject provided by the EMF Compare DiffModel with an EObject from the reference model as parent.
	 * @param element The element which is analyzed
	 * @param target The parent in the reference Model of the analyzed element
	 * @param refID A string which contains the refID for the new Change Object
	 * @param constraint A string which contains a constraint for the new Change Object
	 */
	public final void analyzeEObject(final EObject element, final EObject target, final String refID, final String constraint) {
		EObject 	newTarget 				= null; 	
		EObject		newElement				= element;
		if (target != null) {
			newTarget						= target;
		} else {
			// Only for UML models
			newTarget 						= EMFDeltaUMLHelper.findAddedStereotypeTarget(newElement);
		}
		// Only for UML models
		// If its not a UML model, the same element will be returned
		newElement							= EMFDeltaUMLHelper.convertToStereotypeIfSuitable(newElement);
		EClass 		newMetaClass			= newElement.eClass();
		
		// Set 'parent = null' and 'target = parent' element from the reference model
		AddElement 	newAddElement			= new AddElement(newTarget, newMetaClass, null);
		
		// Create a PossibleSubstitute object corresponding to this AddElement
		PossibleSubstitute possSubst;
		possSubst 							= this.emfDelta.addPossibleSubstitute(newAddElement);
		possSubst.setChaTarget(newElement);
		
		analyzeEObjectDetails(newAddElement, newElement);
		
		Change newChange 					= new Change(refID);
		Alternative newAlternative			= new Alternative();
		newChange.addAlternative(newAlternative);
		this.deltaList.add(newChange);
		newAlternative.addDeltaElement(newAddElement);
		
		if (!EMFDeltaUMLHelper.isStereotype(newElement)) {
			checkForSubEObjects(newAddElement, newElement, newChange);
			// Only for UML models
			EMFDeltaUMLHelper.checkForAddedStereotypes(newAddElement, newElement, newChange, deltaList, this);
		} else {
			try {
				// Only for UML models
				EMFDeltaUMLHelper.checkForAddedTaggedValues(newElement, newChange, EMFDeltaHelper.getChangedModelElement(newTarget), refID, deltaList);
			} catch (MatchElementsUnsetException meue) {
				meue.printStackTrace();
			}
		}
	}
	
	/**
	 * Analyzes a single EObject provided by the EMF Compare DiffModel with an AddElement as parent.
	 * @param element The element which is analyzed
	 * @param parent The parent (other AddElement) in the DeltaModel
	 * @param refID A string which contains the refID for the new Change Object
	 * @param constraint A string which contains a constraint for the new Change Object
	 * @return An AddElement to fill recursively the content of parent elements
	 */
	private AddElement analyzeEObject(final EObject element, final AddElement parent, final String refID, final Change requiredChange) {
		Change newChange = analyzeEObjectAsChange(element, parent, refID, requiredChange);
		AddElement newAddElement = (AddElement) newChange.getAlternatives().get(0).getDeltaElements().get(0);
		return newAddElement;
	}
	
	/**
	 * Analyzes a single EObject provided by the EMF Compare DiffModel with an AddElement as parent.
	 * @param element The element which is analyzed
	 * @param parent The parent (other AddElement) in the DeltaModel
	 * @param refID A string which contains the refID for the new Change Object
	 * @param constraint A string which contains a constraint for the new Change Object
	 * @return A Change object to fill recursively the content of parent elements
	 */
	protected Change analyzeEObjectAsChange(final EObject element, final AddElement parent, final String refID, final Change requiredChange) {
		AddElement 	newParent				= parent;
		EClass 		newMetaClass			= element.eClass();

		// Set parent = parent AddELement and target = null
		AddElement 	newAddElement			= new AddElement(null, newMetaClass, newParent);
		
		analyzeEObjectDetails(newAddElement, element);
		
		Change newChange = new Change(refID);
		if (requiredChange != null) {
			newChange.addConstraint(new ChangeConstraint(ConstraintType.REQ, requiredChange, newChange));
		}
		Alternative newAlternative = new Alternative();
		newChange.addAlternative(newAlternative);
		this.deltaList.add(newChange);
		newAlternative.addDeltaElement(newAddElement);
		
		// Only for UML models
		if (!EMFDeltaUMLHelper.isStereotype(element)) {
			checkForSubEObjects(newAddElement, element, newChange);
		}
		// It is not necessary to check for stereotypes and tagged values at this place, because if they are 
		// applied to an other addition, they will not occur as an own addition in the DiffModel
		
		return newChange;
	}
		
	//########################################################################################
	/**
	 * Provides a more detailed analysis of the EAttributes and EReferences of an EObject and
	 * applies the results to the AddElement.
	 * @param newAddElement The AddElement which is filled with the found informations
	 * @param element The element which will be analyzed
	 */
	private void analyzeEObjectDetails(final AddElement newAddElement, final EObject element) {
		EClass elementClass = element.eClass();
		
		for (EAttribute eA : elementClass.getEAllAttributes()) {
			if (!eA.isDerived()) {
				if (eA.getName().equalsIgnoreCase("name")) {
					Object value = element.eGet(eA);
					newAddElement.addKeyValuePair(eA.getName(), value);
					// Only for UML models
					EMFDeltaUMLHelper.checkForQualifiedName(element, newAddElement);
				}
			}
		}
		
		for (EReference eR : elementClass.getEAllReferences()) {
			if (!eR.isDerived()) {
				Object value = element.eGet(eR);
				newAddElement.addKeyValuePair(eR.getName(), value);
			}
		}
	}
	
	//########################################################################################
	/**
	 * Analyzes the EReferences for SubElements which have to be analyzed.
	 * @param newAddElement The AddElement which is filled with the found informations
	 * @param element The element which will be analyzed
	 * @param refID A string which contains the refID for the new Change Object
	 */
	private void checkForSubEObjects(final AddElement newAddElement, final EObject element, final Change requiredChange) {
		EClass elementClass = element.eClass();
		AddElement cont = null;
		
		for (EReference eR : elementClass.getEAllReferences()) {
			if (!eR.isDerived() && eR.isContainment() && eR.isMany()) {
				@SuppressWarnings("unchecked")
				EList<EObject> valueList = (EList<EObject>) element.eGet(eR);
				int refIDsub = 0;
				if (valueList != null) {
					for (EObject innerElement : valueList) {
						refIDsub++;
						cont = analyzeEObject(innerElement, newAddElement, requiredChange.getRef() + "-" + Integer.toString(refIDsub), requiredChange);
						newAddElement.addContainedElement(cont);
					}
				} else {
					EObject innerElement = (EObject) element.eGet(eR);
					if (innerElement != null) {
						refIDsub = 1;
						cont = analyzeEObject(innerElement, newAddElement, requiredChange.getRef() + "-" + Integer.toString(refIDsub), requiredChange);
						newAddElement.addContainedElement(cont);
					}
				}
			}
		}
	}
}
