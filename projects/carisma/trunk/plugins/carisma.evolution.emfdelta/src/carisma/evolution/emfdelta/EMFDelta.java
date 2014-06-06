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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.compare.diff.metamodel.AttributeChangeRightTarget;
import org.eclipse.emf.compare.diff.metamodel.DiffElement;
import org.eclipse.emf.compare.diff.metamodel.DiffModel;
import org.eclipse.emf.compare.diff.metamodel.DifferenceKind;
import org.eclipse.emf.compare.diff.metamodel.ModelElementChangeLeftTarget;
import org.eclipse.emf.compare.diff.metamodel.ModelElementChangeRightTarget;
import org.eclipse.emf.compare.diff.metamodel.ReferenceChangeLeftTarget;
import org.eclipse.emf.compare.diff.metamodel.ReferenceChangeRightTarget;
import org.eclipse.emf.compare.diff.metamodel.UpdateAttribute;
import org.eclipse.emf.compare.diff.metamodel.UpdateReference;
import org.eclipse.emf.compare.diff.service.DiffService;
import org.eclipse.emf.compare.match.MatchOptions;
import org.eclipse.emf.compare.match.metamodel.MatchModel;
import org.eclipse.emf.compare.match.service.MatchService;
import org.eclipse.emf.compare.util.ModelUtils;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.Change;
import carisma.evolution.DeltaElement;
import carisma.evolution.IDeltaDescriptionGenerator;


/** 
 * An implementation of the IDeltaDescriptionGenerator named EmfDiff.
 * @author Johannes Kowald
 */
public class EMFDelta implements IDeltaDescriptionGenerator {
	
	/**
	 * A ArrayList of changes - the deltaList.
	 */
	private List<Change> deltaList;
	
	/**
	 * An ArrayList of possible Substitutes, which will be checked by searchForSubstitutes(..).
	 */
	private List<PossibleSubstitute> possSubstList;
	
	/**
	 * The EMF Compare DiffModel object.
	 */
	private DiffModel diffModel = null;
	
	/**
	 * The analyzeSubstituteObject instance.
	 */
	private List<AnalyzeSubstituteObject> substituteFinderList; 
	
	/**
	 * List of already analyzed EObjects (to avoid analyzing twice).
	 */
	private List<EObject> analyzedElementsList;
	
	//########################################################################################
	/**
	 * This is the constructor class which initiates the DeltaList object.
	 */
	public EMFDelta() {
		this.deltaList = new ArrayList<Change>();
		this.possSubstList = new ArrayList<PossibleSubstitute>();
		
		// List of Substitute-Finder
		substituteFinderList = new ArrayList<AnalyzeSubstituteObject>();
		AnalyzeSubstituteObject corrLinks = new SubstFinder_CorrespondingLinks(this.deltaList, this.possSubstList);
		substituteFinderList.add(corrLinks);
		
		// List of analyzed EObjects (to avoid analyzing twice)
		analyzedElementsList = new ArrayList<EObject>();
	}
	
	//########################################################################################
	/**
	 * Generates the list of change descriptions
	 * by analyzing the EMF Compare DiffModel.
	 * @return - list of change descriptions 
	 */
	@Override
	public final List<Change> generateDeltaDescriptions() {
		if (diffModel == null) {
			Logger.log(LogLevel.ERROR, "Diffmodel equals null.");
		} else {
			if (!generateDeltaList()) {
				Logger.log(LogLevel.ERROR, "GenerateDiffList failed!"); 
			}
		}
		return Collections.unmodifiableList(this.deltaList);	
	}
	
	//########################################################################################
	/**
	 * Creates the DiffModel via EMF comparing two model files.
	 * @param file1 - A File Object referencing to a model which can be handled by EMF Compare
	 * @param model2 - An EObject referencing to a model which can by handled by EMF Compare
	 * @return - EMF Compare DiffModel
	 */
	public final DiffModel createDiffModel(final File file1, final EObject model2) {
					
		// Defining ResourceSets
		ResourceSet resourceSet2 = new ResourceSetImpl();
		
		try {
			// Set analysis mode
			EMFDeltaHelper.setAnalysisModeByFilename(file1);
			
			// Loading Models for EMF Compare
			EObject model1 = ModelUtils.load(file1, resourceSet2);
			
			// Matching Options
			Map<String, Object> matchoptions = new HashMap<String, Object>();
			matchoptions.put(MatchOptions.OPTION_IGNORE_ID, true);
			matchoptions.put(MatchOptions.OPTION_IGNORE_XMI_ID, true);
			matchoptions.put(MatchOptions.OPTION_DISTINCT_METAMODELS, false);
			
			// Matching model elements
			MatchModel match = MatchService.doMatch(model2, model1, matchoptions);
			EMFDeltaHelper.setActualMatchElementList(match.getMatchedElements());
			
			// Computing differences
			diffModel = DiffService.doDiff(match, false);
						
			return diffModel;
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, "IOException while creating the DiffModel.", e);
			return null;
		} catch (InterruptedException e) {
			Logger.log(LogLevel.ERROR, "InterruptedException while matching the DiffModels.", e);
			return null;
		}
	}
	
	//########################################################################################
	/**
	 * Generates the DiffList by analyzing the Differences provided by EMF Compare DiffModel.
	 * @return - A boolean which indicates the success of the class.
	 */
	private boolean generateDeltaList() {
		
		int refIDCounter = 0;
		
		for (DiffElement de : diffModel.getDifferences()) {
			if (!(de instanceof ReferenceChangeLeftTarget)) {
				refIDCounter++;
				analyseDiffElement(de, Integer.toString(refIDCounter));
			}
		}
		
		//Check the DeltaModel for Substitutes
		for (AnalyzeSubstituteObject substFinder : substituteFinderList) {
			substFinder.searchForSubstitutes(refIDCounter);
		}
		return true;
	}
	
	//########################################################################################
	/**
	 * Analyzes a single DiffElement provided by the EMF Compare DiffModel.
	 * @param diffElement - a single DiffElement provided by the EMF Compare DiffModel
	 * @param refID - a string which contains the refID for the new Change Object
	 */
	private void analyseDiffElement(final DiffElement diffElement, final String refID) {
		//Analyze
		DifferenceKind differenceKind = diffElement.getKind();
		
		if (differenceKind == DifferenceKind.ADDITION) {
			ModelElementChangeLeftTarget mECLT 	= (ModelElementChangeLeftTarget) diffElement;
			EObject 	element					= mECLT.getLeftElement();
			EObject 	target					= mECLT.getRightParent();
			if (!(analyzedElementsList.contains(element))) {
				AnalyzeAddedObject addAnalysis		= new AnalyzeAddedObject(this.deltaList, this);
				addAnalysis.analyzeEObject(element, target, refID, null);
				analyzedElementsList.add(element);
			}
		}
		
		if (differenceKind == DifferenceKind.DELETION) {
			ModelElementChangeRightTarget mECRT = (ModelElementChangeRightTarget) diffElement;
			EObject 	element					= mECRT.getRightElement();
			if (!(analyzedElementsList.contains(element))) {
				AnalyzeDeletedObject delAnalysis	= new AnalyzeDeletedObject(this.deltaList);
				delAnalysis.analyzeEObject(element, refID, null);
				analyzedElementsList.add(element);
			}
		}
		
		if (differenceKind == DifferenceKind.CHANGE) {
			EObject 	element					= null;
			EObject		element2				= null;
			if (diffElement instanceof ReferenceChangeRightTarget) {
				ReferenceChangeRightTarget rCRT 	= (ReferenceChangeRightTarget) diffElement;
				element								= rCRT.getLeftElement();
				element2							= rCRT.getRightElement();
			}
			if (diffElement instanceof AttributeChangeRightTarget) {
				AttributeChangeRightTarget aCLT 	= (AttributeChangeRightTarget) diffElement;
				element								= aCLT.getLeftElement();
				element2							= aCLT.getRightElement();
			}
			if (diffElement instanceof UpdateAttribute) {
				UpdateAttribute uA					= (UpdateAttribute) diffElement;
				element								= uA.getLeftElement();
				element2							= uA.getRightElement();
			}
			if (diffElement instanceof UpdateReference) {
				UpdateReference uR					= (UpdateReference) diffElement;
				element								= uR.getLeftElement();
				element2							= uR.getRightElement();
			}
			if (element != null) {
				if (!(analyzedElementsList.contains(element))) {
					AnalyzeEditObject editAnalysis		= new AnalyzeEditObject(this.deltaList, this);
					editAnalysis.analyzeEObject(element, element2, refID);
					analyzedElementsList.add(element);
				}
			} else {
				Logger.log(LogLevel.ERROR, "Can't find the right cast for diffElement to analyze the Change as EObject");
			}
		}
	}
	
	//########################################################################################
	/**
	 * A method to add a possible substitute.
	 * @param deltaElement - The corresponding DeltaElement
	 * @return the created PossibleSubstitute object
	 */
	public final PossibleSubstitute addPossibleSubstitute(final DeltaElement deltaElement) {
		PossibleSubstitute pS = new PossibleSubstitute(deltaElement);
		possSubstList.add(pS);
		return pS;
	}
	
	//########################################################################################
	/**
	 * A method to remove a possible substitute.
	 * @param possibleSubstitute - the possibleSubstitute object which will be deleted
	 */
	public final void removePossibleSubstitute(final PossibleSubstitute possibleSubstitute) {
		possSubstList.remove(possibleSubstitute);
	}
}
