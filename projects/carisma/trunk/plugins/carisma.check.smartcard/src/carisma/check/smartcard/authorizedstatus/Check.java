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
package carisma.check.smartcard.authorizedstatus;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;

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
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
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
		if (!(currentModel.getContents().get(0) instanceof Model)) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
		boolean noErrors = true;
		Model model = (Model) currentModel.getContents().get(0);
		AuthorizedStatusCheck authorizedCheck = new AuthorizedStatusCheck();
		if (authorizedCheck.checkAllAuthorizedStates(model) > 0) {
			for (AnalysisMessage errorMessage : authorizedCheck.getErrorMessages()) {
				if (errorMessage.getType() == StatusType.ERROR) {
					noErrors = false;
					break;
				}
			}
			for (AnalysisMessage errorMessage : authorizedCheck.getErrorMessages()) {
				errorMessage.print(newHost,null);
			}
			return noErrors;
		}
		AnalysisMessage successfulMessage = new AnalysisMessage(StatusType.INFO, OutputTarget.BOTH, Messages.checkSuccessful());
		successfulMessage.print(newHost, null);
		return true;
	}
	
}
