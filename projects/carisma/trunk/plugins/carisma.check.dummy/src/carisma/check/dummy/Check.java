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
package carisma.check.dummy;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;


public class Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.dummy";
	public static final String PARAM_STRING = "carisma.check.dummy.testString";
	public static final String PARAM_NUMBER = "carisma.check.dummy.number";
	public static final String CHECK_NAME = "CARiSMA Dummy Check";

	private AnalysisHost analysisHost = null;
	private int numOfElements = 0;
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		this.analysisHost = host;
		this.numOfElements = 0;
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			Package model = (Package) currentModel.getContents().get(0);
			printContent(model, "");
			for (Stereotype e : UMLHelper.getAllElementsOfType(model, Stereotype.class)) {
				host.appendLineToReport(e.toString());
			}
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "number of elements counted: "+this.numOfElements));
			return true;
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
	}
	
	public void printContent(Element element, String indent) {
		this.numOfElements++;
		this.analysisHost.appendToReport(indent+element.eClass().getName()+": ");
		if (!element.getAppliedStereotypes().isEmpty()) {
			this.analysisHost.appendToReport("<<");
			for (Stereotype st : element.getAppliedStereotypes()) {
				this.analysisHost.appendToReport(st.getName()+",");
			}
			this.analysisHost.appendToReport(">> ");
		}
		if (element instanceof NamedElement) {
			NamedElement namedElement = (NamedElement)element;
			this.analysisHost.appendToReport(namedElement.getName());
		}
		this.analysisHost.appendLineToReport("");
		for (Element child : element.allOwnedElements()) {
			printContent(child, indent+"  ");
		}
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
