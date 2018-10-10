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
package carisma.check.emfdelta;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.compare.diff.metamodel.AttributeChangeRightTarget;
import org.eclipse.emf.compare.diff.metamodel.DiffElement;
import org.eclipse.emf.compare.diff.metamodel.DiffModel;
import org.eclipse.emf.compare.diff.metamodel.DifferenceKind;
import org.eclipse.emf.compare.diff.metamodel.ModelElementChangeLeftTarget;
import org.eclipse.emf.compare.diff.metamodel.ModelElementChangeRightTarget;
import org.eclipse.emf.compare.diff.metamodel.ReferenceChangeRightTarget;
import org.eclipse.emf.compare.diff.metamodel.UpdateAttribute;
import org.eclipse.emf.compare.diff.metamodel.UpdateContainmentFeature;
import org.eclipse.emf.compare.diff.metamodel.UpdateModelElement;
import org.eclipse.emf.compare.diff.metamodel.UpdateReference;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;


/** 
 * A SubClass of the (Help-)Analysis Plugin for the CARiSMA Tool to check the 
 * EmfDiff implementation of the IDetlaDescriptionGenerator.
 * @author Johannes Kowald
 */
public class DumpEMFCompare {
	
	/**
	 * The parameter AnalysisHost.
	 */
	private AnalysisHost host;
	
	/** 
	 * Constant String for a fix amount of spaces.
	 */
	private static final String SPACE = "   ";
	
	/**
	 * The success of the dumping.
	 */
	private boolean success = false;
	
	//########################################################################################
	/**
	 * This method calls recursively the DumpDiffElement method to Dump all DiffElements
	 * provided the EMF Compare DiffModel.
	 * @param dm - The DiffModel provided by EMF Compare
	 * @param host - The AnalysisHost Object to deliver Analysis results
	 */
	public DumpEMFCompare(final DiffModel dm, final AnalysisHost host) {
		this.host = host;
		host.appendLineToReport("###################################");
		host.appendLineToReport("###   EMF COMPARE DIFF-MODEL    ###");
		host.appendLineToReport("###################################");
		for (DiffElement de : dm.getDifferences()) {
			dumpDiffElement(de, "");
		}
		host.appendLineToReport("");
		this.success = true;
	}
	
	//########################################################################################
	/**
	 * Returns the success state of the dump class.
	 * @return - A boolean which indicates the success of the DumpMethod
	 */
	public final boolean getSuccess() {
		return this.success;
	}
	
	//########################################################################################
	/**
	 * This method dumps all content of a DiffElement provided by the EMF Compare DiffModel.
	 * @param de - A single DiffElement provided by the EMF Compare DiffModel
	 * @param indent - A String to handle the indenting of the result text
	 */
	private void dumpDiffElement(final DiffElement de, final String indent) {
		host.appendLineToReport("### DIFFELEMENT");
		host.appendLineToReport(indent + "SubDiffs = " + de.getSubDiffElements().size());
		DifferenceKind dk = de.getKind();
		host.appendLineToReport(indent + "DifferenceKind = " + dk.getName());
		
		if (dk == DifferenceKind.ADDITION) {
			ModelElementChangeLeftTarget castde1 = (ModelElementChangeLeftTarget) de;
			EObject e = castde1.getLeftElement();
			dumpEObject(e, indent + SPACE, "LeftElement");
		}
		
		if (dk == DifferenceKind.DELETION) {
			ModelElementChangeRightTarget castde2 = (ModelElementChangeRightTarget) de;
			EObject e = castde2.getRightElement();
			dumpEObject(e, indent + SPACE, "RightElement");
		}
		
		if (dk == DifferenceKind.CHANGE) {
			EObject 	element						= null;
			if (de instanceof ReferenceChangeRightTarget) {
				ReferenceChangeRightTarget rCRT 	= (ReferenceChangeRightTarget) de;
				element								= rCRT.getLeftElement();
			}
			if (de instanceof AttributeChangeRightTarget) {
				AttributeChangeRightTarget aCRT		= (AttributeChangeRightTarget) de;
				element								= aCRT.getLeftElement();
			}
			if (de instanceof UpdateAttribute) {
				UpdateAttribute uA					= (UpdateAttribute) de;
				element								= uA.getLeftElement();
			}
			if (de instanceof UpdateContainmentFeature) {
				UpdateContainmentFeature uCF		= (UpdateContainmentFeature) de;
				element								= uCF.getLeftElement();
			}
			if (de instanceof UpdateModelElement) {
				UpdateModelElement uME				= (UpdateModelElement) de;
				element								= uME.getLeftElement();
			}
			if (de instanceof UpdateReference) {
				UpdateReference uR					= (UpdateReference) de;
				element								= uR.getLeftElement();
			}
			if (element != null) {
				dumpEObject(element, indent + SPACE, "LeftElement");
			}
		}
		
		for (DiffElement de2 : de.getSubDiffElements()) {
			dumpDiffElement(de2, indent + "	S:");
		}
	}
	
	//########################################################################################	
	/**
	 * This method dumps all content of a EObject, mostly a single DiffElement casted to an EObject.
	 * @param eobject - A single DiffElement or property of a DiffElement casted to an EObject
	 * @param indent - A String to handle the indenting of the result text
	 * @param headline - A String to define a sub result headline
	 */
	private void dumpEObject(final EObject eobject, final String indent, final String headline) {		
		EClass ec = eobject.eClass();
				
		host.appendLineToReport(indent + "# " + headline + "->EAttributes");
		for (EAttribute ea : ec.getEAllAttributes()) {
			Object value = eobject.eGet(ea);
			if (!ea.isDerived()) {
				host.appendLineToReport(indent + "		" + ea.getName() + " = " + value);
			}
		}
		
		host.appendLineToReport(indent + "# " + headline + "->EReference");
		for (EReference er : ec.getEAllReferences()) {
			Object value = eobject.eGet(er);
			if (!er.isDerived()) {
				host.appendLineToReport(indent + "		" + er.getName() + " = " + value);
				if (er.isContainment()) {
					if (er.isMany()) {
						@SuppressWarnings("unchecked")
						EList<EObject> valuelist = (EList<EObject>) eobject.eGet(er);
						if (valuelist != null) {
							for (EObject invalue : valuelist) {
								dumpEObject(invalue, indent + SPACE, headline + "->ReferredEObject");
							}
						}
					} else {
						EObject invalue = (EObject) eobject.eGet(er);
						if (invalue != null) {
							dumpEObject(invalue, indent + SPACE, headline + "->ReferredEObject");
						}
					}
				}
			}
		}
	}
	
}
