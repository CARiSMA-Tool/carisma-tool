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
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.Change;
import carisma.evolution.emfdelta.EMFDelta;


/** 
 * Wraps the EMF Delta Parser in a CARiSMA Check and puts the results in the register.
 */

public class EMFDeltaParser implements CarismaCheck {
	
	private File otherModel = null;
		
	private static final String CHANGE_REGISTER_NAME = "carisma.data.evolution.changes";
	

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
	    AnalysisHost host; 
	    if (newHost == null) {
	        host = new DummyHost(true);
	    } else {
	        host = newHost;
	    }
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel == null) {
			return false;
		}
		EObject modelRoot = currentModel.getContents().get(0);
		extractParameters(parameters);
		if (otherModel == null || modelRoot == null) {
			return false;
		}
		EMFDelta theParser = new EMFDelta();
		theParser.createDiffModel(otherModel, modelRoot);
		List<Change> changes = theParser.generateDeltaDescriptions();
		try {
			if (host.isRegisterInUse(CHANGE_REGISTER_NAME)) {
			    host.removeFromRegister(CHANGE_REGISTER_NAME);
			}
		} catch (RegisterNotInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
		try {
		    host.putToRegister(CHANGE_REGISTER_NAME, changes);
		} catch (RegisterInUseException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Parsed model for Changes with the EMFDelta Parser..."));
		return true;
	}
	
	private void extractParameters(final Map<String, CheckParameter> parameters) {
		if (!parameters.isEmpty()) {
			InputFileParameter otherModelParameter = (InputFileParameter) parameters.get("carisma.evolution.emfdelta.referenceModel");
			if (otherModelParameter != null) {
				otherModel = otherModelParameter.getValue();
			}
		}
	}
	
}