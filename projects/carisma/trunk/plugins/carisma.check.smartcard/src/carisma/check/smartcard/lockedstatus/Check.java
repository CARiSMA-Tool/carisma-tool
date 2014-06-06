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
package carisma.check.smartcard.lockedstatus;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Package;

import carisma.check.smartcard.utils.AnalysisMessage;
import carisma.check.smartcard.utils.OutputTarget;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;


public class Check implements CarismaCheck {

	private AnalysisHost host;
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost newHost) {
		if (newHost != null) {
			host = newHost;			
		} else {
			host = new DummyHost(true);
		}
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (!(currentModel.getContents().get(0) instanceof Package)) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
		boolean noErrors = true;
		Package model = (Package) currentModel.getContents().get(0);
		LockedStatusCheck lockedCheck = new LockedStatusCheck();
		if (lockedCheck.startCheck(model) > 0) {
			for (AnalysisMessage errorMessage : lockedCheck.getErrorMessages()) {
				if (errorMessage.getType() == StatusType.ERROR) {
					noErrors = false;
					break;
				}
			}
			for (AnalysisMessage errorMessage : lockedCheck.getErrorMessages()) {
				errorMessage.print(newHost, null);
			}
			return noErrors;
		}
		AnalysisMessage successfulCheck =
				new AnalysisMessage(StatusType.INFO, OutputTarget.BOTH, Messages.allLockedStatesCorrect());
		successfulCheck.print(newHost, null);
		return true;
	}
	

}
