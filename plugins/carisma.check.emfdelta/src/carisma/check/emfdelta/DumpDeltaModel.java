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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.eclipse.emf.ecore.EObject;

import carisma.core.analysis.AnalysisHost;
import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.ChangeConstraint;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;


/** 
 * A SubClass of the (Help-)Analysis Plugin for the CARiSMA Tool to check the 
 * EmfDiff implementation of the IDeltaDescriptionGenerator.
 * @author Johannes Kowald
 */
public class DumpDeltaModel {

	/**
	 * The parameter AnalysisHost.
	 */
	private AnalysisHost host;
	
	/**
	 * Constant String for output.
	 */
	private static final String LINE =  "       - ";
	/**
     * Constant String for output.
     */
    private static final String SUB_LINE = "         - ";
	
	/**
	 * Constant String for output.
	 */
	private static final String TARGET = "Target: ";
	
	/**
	 * The success of the dumping.
	 */
	private boolean success = false;
	
	//########################################################################################
	/**
	 * This method calls the dumpChange method to dump all Changes
	 * provided the DeltaModel.
	 * @param changeList - The List of Changes of the DeltaModel
	 * @param host - The AnalysisHost Object to deliver Analysis results
	 */
	public DumpDeltaModel(final List<Change> changeList, final AnalysisHost host) {
		this.host = host;
		host.appendLineToReport("###################################");
		host.appendLineToReport("###         DELTA MODEL         ###");
		host.appendLineToReport("###################################");
		int counter = 0;
		for (Change change : changeList) {
			counter++;
			host.appendLineToReport("# CHANGE " + counter);
			host.appendLineToReport("   refID       = '" + change.getRef() + "'");
			host.appendLineToReport("   constraints = " + dumpStringArray(change.getConstraints()));
			dumpChange(change);
		}
		host.appendLineToReport("###################################");
		host.appendLineToReport("###           THE END           ###");
		host.appendLineToReport("###################################");
		
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
	 * This method calls the dumpAlternative method to dump all Alternatives
	 * provided the DeltaModel.
	 * @param change - A single Change Object
	 */
	private void dumpChange(final Change change) {
		int counter = 0;
		for (Alternative alternative : change.getAlternatives()) {
			counter++;
			host.appendLineToReport("   # Alternative " + counter);
			dumpAlternative(alternative);
		}
	}
	
	//########################################################################################
	/**
	 * This method calls the dumpDeltaElement method to dump all DeltaElements
	 * provided the DeltaModel.
	 * @param alternative - A single Alternative Object
	 */
	private void dumpAlternative(final Alternative alternative) {
		for (DeltaElement deltaElement : alternative.getDeltaElements()) {
			dumpDeltaElement(deltaElement);
		}
	}
	
	//########################################################################################
	/**
	 * This method calls the different dumpElement methods to dump all Elements
	 * provided the DeltaModel.
	 * @param deltaElement - A single DeltaElement Object
	 */
	private void dumpDeltaElement(final DeltaElement deltaElement) {
		if (deltaElement instanceof AddElement) {
			AddElement addElement = (AddElement) deltaElement;
			
			host.appendLineToReport("      ## AddElement found!");
			host.appendLineToReport(LINE + "MetaClass: " + addElement.getMetaClass());
			if (addElement.getParent() != null) {
				host.appendLineToReport(LINE + "Parent: " + addElement.getParent().toString());
			}
			if (addElement.getTarget() != null) {
				host.appendLineToReport(LINE + TARGET + addElement.getTarget().toString());
			}
			Iterator<Map.Entry<String, Object>> iterator = addElement.getValues().entrySet().iterator();
			while (iterator.hasNext()) {
				Map.Entry<String, Object> entry = (Map.Entry<String, Object>) iterator.next();
				if (entry != null) {
					if (entry.getValue() != null) {
						host.appendLineToReport(LINE + entry.getKey() + " = " + entry.getValue().toString());
				
					} else {
						host.appendLineToReport(LINE + entry.getKey() + " = null");
					}
				}
			}
			if (addElement.getContent().size() > 0) {
				host.appendLineToReport(LINE + "Content:");
				for (AddElement adde : addElement.getContent()) {
					host.appendLineToReport(SUB_LINE + adde.toString());
				}
			}
			host.appendLineToReport("");
		}
		
		if (deltaElement instanceof DelElement) {
			DelElement delElement = (DelElement) deltaElement;
			
			host.appendLineToReport("      ## DelElement found!");
			if (delElement.getTarget() != null) {
				host.appendLineToReport(LINE + TARGET + delElement.getTarget().toString());
			}
			if (delElement.getAccompanyingDeletions().size() > 0) {
				host.appendLineToReport(LINE + "accompanyingDeletions:");
				for (EObject accDel : delElement.getAccompanyingDeletions()) {
					host.appendLineToReport(SUB_LINE + accDel.toString());
				}
			}
			host.appendLineToReport("");
		}
		
		if (deltaElement instanceof EditElement) {
			EditElement editElement = (EditElement) deltaElement;
			
			host.appendLineToReport("      ## EditElement found!");
			if (editElement.getTarget() != null) {
				host.appendLineToReport(LINE + TARGET + editElement.getTarget().toString());
			}
			Iterator<Map.Entry<String, Object>> iterator = editElement.getValues().entrySet().iterator();
			while (iterator.hasNext()) {
				Map.Entry<String, Object> entry = (Map.Entry<String, Object>) iterator.next();
				if (entry != null) {
					if (entry.getValue() != null) {
						host.appendLineToReport(LINE + entry.getKey() + " = " + entry.getValue().toString());
				
					} else {
						host.appendLineToReport(LINE + entry.getKey() + " = null");
					}
				}
			}
			host.appendLineToReport("");
		}
		
		if (deltaElement instanceof SubstElement) {
			SubstElement substElement = (SubstElement) deltaElement;
			
			host.appendLineToReport("      ## SubstElement found!");
			if (substElement.getTarget() != null) {
				host.appendLineToReport(LINE + TARGET + substElement.getTarget().toString());
			}
			host.appendLineToReport(LINE + "Component:");
			for (AddElement addE : substElement.getComponents()) {
				host.appendLineToReport(SUB_LINE + addE.toString());
			}
			host.appendLineToReport(LINE + "accompanyingDeletions:");
			for (EObject delE : substElement.getAccompanyingDeletions()) {
				host.appendLineToReport(SUB_LINE + delE.toString());
			}
			host.appendLineToReport("");
		}
	}
	
	//########################################################################################
	/**
	 * This method dumps a StringArray as [String1,String2,...].
	 * @param stringarray - The string array which will be dumped
	 * @return String - A string which can be printed
	 */
	private String dumpStringArray(final Collection<ChangeConstraint> changeConstraints) {
		StringBuffer returnstring = new StringBuffer("[");
		for (ChangeConstraint constraint : changeConstraints) {
			if (constraint != null) {
				String constraintString = constraint.toString();
				returnstring.append(constraintString);
				returnstring.append(",");
			}
		}
		returnstring = new StringBuffer(returnstring.substring(0, returnstring.length() - 1));
		if (returnstring.length() != 0) {
			returnstring.append("]");
		} else {
			returnstring = new StringBuffer("empty");
		}
		return returnstring.toString();
	}
	
}
