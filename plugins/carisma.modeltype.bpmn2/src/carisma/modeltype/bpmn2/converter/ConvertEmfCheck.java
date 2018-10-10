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
package carisma.modeltype.bpmn2.converter;

import java.io.File;
import java.util.Map;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.bpmn2.yaoqiang.YaoqiangHelper;

/**
 * This Check transforms an EMF BPMN 2.0 to a Yaoqiang model.
 * @author Marcel Michel
 *
 */
public class ConvertEmfCheck implements CarismaCheckWithID {

	private static final String CHECK_ID = "carisma.modeltype.bpmn2.emfconverter";
	private static final String PARAM_OUTPUTMODEL = "carisma.modeltype.bpmn2.outputmodel";

	private static final String CHECK_NAME = "EMF to Yaoqiang Converter";
	
	private static final String ANALYSISMODEL = "analysismodel";
	
	/**
	 * Performs conversion.
	 * @param parameters The Checkparameters
	 * @param host The AnalysisHost
	 * @return If successful true otherwise false
	 */
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		
		File modelFile = new File(host.getAnalyzedModel().getURI().toFileString());
		
		File outputFile = null;
		//TODO: Check this chains
		if (parameters.containsKey(PARAM_OUTPUTMODEL)) {
			outputFile = ((OutputFileParameter) parameters.get(PARAM_OUTPUTMODEL)).getValue();
			if (outputFile != null) {
				if (outputFile.getName().equalsIgnoreCase(ANALYSISMODEL)) {
					outputFile = modelFile;
				}
			} else {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
						"Output File is not defined. To overwrite the default model use 'AnalysisModel'"));
			}
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
				"Could not resolve necessary parameters!"));
			return false;
		}
		
		
		if (YaoqiangHelper.emf2yaoqiangModel(modelFile.getAbsolutePath(), 
				outputFile.getAbsolutePath())) {
			return true;
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
			"Error during conversion!"));
		return false;
	}

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}
}