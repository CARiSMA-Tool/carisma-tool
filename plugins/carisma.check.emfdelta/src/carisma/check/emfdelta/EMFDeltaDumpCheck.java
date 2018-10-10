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

import java.io.File;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.compare.diff.metamodel.DiffModel;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.evolution.Change;
import carisma.evolution.IDeltaDescriptionGenerator;
import carisma.evolution.emfdelta.EMFDelta;


/** 
 * A (Supporting-)Check for CARiSMA, which dumps the EMF-Compare Diffmodel
 * and the corresponding informations in the CARiSMA Deltamodel. It provides
 * a possibility to check the EMFDelta implementation of the 
 * IDeltaDescriptionGenerator.
 * @author Johannes Kowald
 */
public class EMFDeltaDumpCheck implements CarismaCheck {

	/**
	 * The AnalysisHost to write reports etc.
	 */
	private AnalysisHost host = null;
	
	/**
	 * run method which runs the check and provides main functions of the check.
	 * @param parameters - A list of PluginParameters
	 * @param host - The AnalysisHost Object to deliver Analysis results
	 * @return - A boolean which indicates the success of the CarismaCheck
	 */
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		this.host = host;
		File parameterfile;
		Resource clickedModel;
		
		host.appendLineToReport("### EMF DIFFMODEL AND CARiSMA DELTAMODEL DUMP (by Johannes Kowald)");
		host.appendLineToReport(""); 
		
		try {
			// Ausgangsmodell (Parametermodell)
			InputFileParameter fp = (InputFileParameter) parameters.get("carisma.check.emfdelta.referenceModel");
			parameterfile = fp.getValue();
			
			// Analysemodell (im Package Explorer gewähltes Modell)
			clickedModel = host.getAnalyzedModel();
			
			// Modeldateien prüfen
			if (!checkModel(parameterfile)) {
				return false;
			}
			if (!checkModel(clickedModel)) {
				return false;
			}
			
			
		} catch (Exception e) {
			host.appendLineToReport(e.toString());
			return false;
		}
		IDeltaDescriptionGenerator descrGen			= new EMFDelta();
		DiffModel diffmodel 						= ((EMFDelta)descrGen).createDiffModel(parameterfile, clickedModel.getContents().get(0));
		
		List<Change> changeList	= descrGen.generateDeltaDescriptions();
		
		DumpEMFCompare dumpEMFCompare = new DumpEMFCompare(diffmodel, host);
		host.appendLineToReport(""); 
		DumpDeltaModel dumpDeltaModel = new DumpDeltaModel(changeList, host);
		
		if (!dumpEMFCompare.getSuccess()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "DumpEMFCompare exits with an error"));
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "DumpEMFCompare quitted successful"));
		}
		if (!dumpDeltaModel.getSuccess()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "DumpDeltaModel exits with an error"));
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "DumpDeltaModel quitted successful"));
		}
		
		return (dumpEMFCompare.getSuccess() && dumpDeltaModel.getSuccess());
	}
	
	/**
	 * If the Parameter File is readable it returns true for success. 
	 * It also provides readability informations to the AnalyseHost.
	 * @param file - A File Object which is check for its readability
	 * @return - A Boolean which indicates the success of the file check.
	 */
	private boolean checkModel(final File file) {
		if (!file.canRead()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "The parameter model cannot be loaded"));
			return false;
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Parameter model loaded: " + file.getAbsolutePath()));
			return true;
		}
	}
	
	/**
	 * If the Parameter File is readable it returns true for success. 
	 * It also provides readability informations to the AnalyseHost.
	 * @param Resource - A Resource Object which is check for its contents
	 * @return - A Boolean which indicates the success of the file check.
	 */
	private boolean checkModel(final Resource resource) {
		if (resource.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Analysis model cannot be loaded"));
			return false;
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Analysis model loaded"));
			return true;
		}
	}
	
	

}
