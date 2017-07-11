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
package carisma.check.staticcheck.securelinks;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Package;

import carisma.check.staticcheck.securelinks.utils.AnalysisMessage;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;


/**
 * standart plugin for Carisma.
 * @author Klaus Rudack
 *
 */
public class SecureLinksCheck implements CarismaCheckWithID {
	
	public static final String CHECK_ID = "carisma.check.staticcheck.securelinks";
	public static final String CHECK_NAME = "UMLsec secure links Check";

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
	    AnalysisHost host;
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
		Package model = (Package) currentModel.getContents().get(0);
		SecureLinks check = new SecureLinks(host);
		if (check.checkSecureLinks(model) > 0) {
			for (AnalysisMessage errorMessage : check.getErrorMessages()) {
				if (errorMessage.getType() == StatusType.ERROR) {
					noErrors = false;
					break;
				}
			}
			for (AnalysisMessage errorMessage : check.getErrorMessages()) {
				errorMessage.print(host);
			}
			return noErrors;
		}
		return true;
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
