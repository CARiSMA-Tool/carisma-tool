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
import java.util.List;

import org.eclipse.emf.ecore.EObject;

import org.eclipse.emf.compare.match.metamodel.MatchElement;
import org.eclipse.emf.compare.match.metamodel.impl.Match2ElementsImpl;

/**
 * Provides helping methods to analyze differences between two models.
 * @author Johannes Kowald
 *
 */
public final class EMFDeltaHelper {
	/**
	 * Constant int for unknown analysis mode.
	 */
	public static final int ANALYSIS_MODE_UNKNOWN = 0;
	
	/**
	 * Constant int for UML analysis mode.
	 */
	public static final int ANALYSIS_MODE_UML = 1;
	
	/**
	 * Constant int for UML analysis mode.
	 */
	public static final int ANALYSIS_MODE_BPMN = 2;
	
	/**
	 * The analysis mode.
	 */
	private static int analysis_mode = 0;
	
	/**
	 * The actual list of matched elements
	 */
	private static List<MatchElement> matchElements;
	
	//########################################################################################
	/**
	 * Sets the analysis mode.
	 */
	public static void setAnalysisModeByFilename(File file) {
		analysis_mode = ANALYSIS_MODE_UNKNOWN;
		String file_extension = file.getName().substring(file.getName().lastIndexOf("."));
		file_extension = file_extension.toLowerCase();
		if (file_extension.contains("uml")) {
			analysis_mode = ANALYSIS_MODE_UML;
		}
		if (file_extension.contains("bpmn")) {
			analysis_mode = ANALYSIS_MODE_BPMN;
		}
	}
	
	//########################################################################################
	/**
	 * Return the actual analysis mode.
	 */
	public static int getAnalysisMode() {
		return analysis_mode;
	}
	
	//########################################################################################
	/**
	 * Sets the list of matched elements to the given list.
	 * @param actualMatchElements The given list of matched elements
	 */
	public static void setActualMatchElementList(List<MatchElement> actualMatchElements) {
		matchElements = actualMatchElements;
	}

	//########################################################################################
	/**
	 * Tries to find the element of the reference model which was matched to the given element of the changed model.
	 * @param changedModelElement The element of the changed model
	 * @return The element of the reference model which was matched to the given element of the changed model
	 */
	public static EObject getReferenceModelElement(EObject changedModelElement) throws MatchElementsUnsetException {
		if (matchElements != null) {
			EObject referenceModelElement = null;
			for (MatchElement matchElement : matchElements) {
				referenceModelElement = getReferenceModelSubElement(matchElement, changedModelElement);
				if (referenceModelElement != null) {
					return referenceModelElement;
				}
			}
		} else {
			throw new MatchElementsUnsetException();
		}
		return null;
	}
	
	//########################################################################################
	/**
	 * Tries to find the element of the reference model which was matched to the given element of the changed model 
	 * in the list of SubMatchElements.
	 * @param matchElement The given MatchElement which possibly holds SubMatchElements
	 * @param changedModelElement The element of the changed model
	 * @return The element of the reference model which was matched to the given element of the changed model
	 */
	private static EObject getReferenceModelSubElement(MatchElement matchElement, EObject changedModelElement) {
		EObject referenceModelElement = null;
		if (matchElement instanceof Match2ElementsImpl) {
			Match2ElementsImpl m2e = (Match2ElementsImpl) matchElement;
			// RightElement = Element of the reference model
			// LeftElement = Element of the changed model
			if (m2e.getLeftElement().equals(changedModelElement)) {
				return m2e.getRightElement();
			}
		}
		for (MatchElement subMatchElement : matchElement.getSubMatchElements()) {
			referenceModelElement = getReferenceModelSubElement(subMatchElement, changedModelElement);
			if (referenceModelElement != null) 
				return referenceModelElement;
		}
		return null;
	}
	
	//########################################################################################
	/**
	 * Tries to find the element of the changed model which was matched to the given element of the reference model.
	 * @param referenceModelElement The element of the reference model
	 * @return The element of the changed model which was matched to the given element of the reference model
	 */
	public static EObject getChangedModelElement(EObject referenceModelElement) throws MatchElementsUnsetException {
		if (matchElements != null) {	
			EObject changedModelElement = null;
			for (MatchElement matchElement : matchElements) {
				changedModelElement = getChangedModelSubElement(matchElement, referenceModelElement);
				if (changedModelElement != null) {
					return changedModelElement;
				}
			}
		} else {
			throw new MatchElementsUnsetException();
		}
		return null;
	}
	
	//########################################################################################
	/**
	 * Tries to find the element of the changed model which was matched to the given element of the reference model 
	 * in the list of SubMatchElements.
	 * @param matchElement The given MatchElement which possibly holds SubMatchElements
	 * @param referenceModelElement The element of the reference model
	 * @return The element of the changed model which was matched to the given element of the reference model
	 */
	private static EObject getChangedModelSubElement(MatchElement matchElement, EObject referenceModelElement) {
		EObject changedModelElement = null;
		if (matchElement instanceof Match2ElementsImpl) {
			Match2ElementsImpl m2e = (Match2ElementsImpl) matchElement;
			// RightElement = Element of the reference model
			// LeftElement = Element of the changed model
			if (m2e.getRightElement().equals(referenceModelElement)) {
				return m2e.getLeftElement();
			}
		}
		for (MatchElement subMatchElement : matchElement.getSubMatchElements()) {
			changedModelElement = getChangedModelSubElement(subMatchElement, referenceModelElement);
			if (changedModelElement != null) 
				return changedModelElement;
		}
		return null;
	}
}
