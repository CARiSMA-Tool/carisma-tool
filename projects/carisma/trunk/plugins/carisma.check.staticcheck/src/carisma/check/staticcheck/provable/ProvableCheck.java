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
package carisma.check.staticcheck.provable;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * 
 * @author Klaus Rudack
 *
 */
//TODO KR: Java Doc
public class ProvableCheck implements CarismaCheck {

	/**
	 * the model to check.
	 */
	private Package model = null;
	
	/**
	 * AnalysisHost for report.
	 */
    private AnalysisHost host;
    
	
	
	/**
	 * main function that starts the check.
	 * @return true if the model is correct according to <<provable>>, false otherwise
	 */
	private boolean startCheck() {
		ArrayList<Element> provableList = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(model, UMLsec.PROVABLE);
		if ((provableList == null) || provableList.size() < 1) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "No Stereotype <<provable>> is applied"));
			host.appendLineToReport("No Stereotype <<provable>> is applied");
			return true;
		}
		return true;
	}
	
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
			host.appendLineToReport("Empty model");
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			model = (Package) currentModel.getContents().get(0);
			return startCheck();
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		host.appendLineToReport("Content is not a model!");
		return false;
	}	
	
}
	
	
