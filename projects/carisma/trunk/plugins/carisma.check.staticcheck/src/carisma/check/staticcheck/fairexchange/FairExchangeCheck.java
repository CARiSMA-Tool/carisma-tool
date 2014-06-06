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
package carisma.check.staticcheck.fairexchange;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * analyzes an activity diagram with respect to fair exchange.
 * @author Klaus Rudack
 *
 */
public class FairExchangeCheck implements CarismaCheck {

	/**
	 * variable to save if an error occurred.
	 */
	private boolean hasError = false;
	
	/**
	 * the given model.
	 */
	private Package elementWithFairExchange = null;
	
	/**
	 * ArrayList with lists that represents the paths through the given ActivityDiagram.
	 */
	private List<List<Element>> pathsList = new ArrayList<List<Element>>();
	
	/**
	 * The given Stereotype fair-exchange.
	 */
	private Stereotype stereotype = null;
	
	/**
	 * AnalysisHost for report.
	 */
	private AnalysisHost host = new DummyHost(true);
	
	/**
	 * default constructor.
	 */
	public FairExchangeCheck() { }
	
	/**
	 * starts the analysis of an activity diagram with the respect to FairExchange.
	 * @param givenModel model that has an activity diagram that should be analyzed in it
	 * @return boolean whether the check was correct or not
	 */
	private boolean fairExchangeAnalysis(final Package givenModel) {
		String fairName = "UMLsec::fair exchange";
			elementWithFairExchange = givenModel;
		if (givenModel == null) {
			AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.WARNING, "The given Model is null.");
			host.addResultMessage(analysisResultMessage);
			return false;
		}
		stereotype = elementWithFairExchange.getAppliedStereotype(fairName);
		if (stereotype == null) {
			List<Element> stereotypeList = UMLsecUtil.getStereotypedElements(givenModel, UMLsec.FAIR_EXCHANGE);
			if ((stereotypeList == null) || (stereotypeList.size() < 1)) {
				host.appendLineToReport("No stereotype <<fair-exchange>> applied.");
				AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.INFO,
						"No stereotype <<fair-exchange>> applied.");
				host.addResultMessage(analysisResultMessage);
			} else {
				for (Element fairExchangedElement : stereotypeList) {
					FairExchangeCheck fe = new FairExchangeCheck();
					fe.fairExchangeAnalysis((Package) fairExchangedElement);
				}
			}
		} else {
			if (!checkStereotype()) {
				return false;
			}
				ActivityDiagramManager adm = new ActivityDiagramManager(givenModel, host);
				pathsList = adm.getAllPaths();
				if (pathsList.size() < 1) {
					host.appendLineToReport("No possible way through this ActivityDiagramm.");
					AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.INFO,
							"No possible way through this ActivityDiagramm.");
					host.addResultMessage(analysisResultMessage);
				} else {
					startAnalysis(host);
				}
		}
		if (!hasError) {
			host.appendLineToReport("Check successfull with respect to <<fair exchange>>.");
			AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.INFO,
					"Test successfull with respect to <<fair-exchange>>.");
			host.addResultMessage(analysisResultMessage);
		}
		return !hasError;
	}
	
	/**
	 * checks if all tags of the Stereotype got content.
	 * @return true if all tags got content, false otherwise
	 */
	@SuppressWarnings("unchecked")
	private boolean checkStereotype() {
		if (((List<List<Element>>) elementWithFairExchange.getValue(stereotype, "start")).size() < 1) {
			host.appendLineToReport("No start-Elements are defined.");
			host.appendLineToReport("Check failed with respect to fair exchange.");
			AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.WARNING,
					"No start-Elements are defined.");
			host.addResultMessage(analysisResultMessage);
			return false;
		}
		if (((List<List<Element>>) elementWithFairExchange.getValue(stereotype, "stop")).size() < 1) {
			host.appendLineToReport("No stop-Elements are defined.");
			host.appendLineToReport("Check failed with respect to fair exchange.");
			AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.ERROR,
					"No stop-Elements are defined.");
			host.addResultMessage(analysisResultMessage);
			return false;
		}
		return true;
	}
	
	/**
	 * analyzes the paths of the given ActivityDiagram.
	 * @param host AnalysisHost for report
	 */
	private void startAnalysis(final AnalysisHost host) {
		EObjectResolvingEList<?> startList = (EObjectResolvingEList<?>) elementWithFairExchange.getValue(stereotype, "start");
		EObjectResolvingEList<?> stopList = (EObjectResolvingEList<?>) elementWithFairExchange.getValue(stereotype, "stop");
		for (List<Element> path : pathsList) {
			boolean found = false;
			for (int j = path.size() - 1; j >= 0; j--) {
				if (!found) {
					if (stopList.contains(path.get(j))) {
						found = true;
						break;
					}
					if (startList.contains(path.get(j)) && !(found)) { /*hier muss mit Inidzes gearbeitet werden, da die Liste von hinten
																		nach vorne durchlaufen wird*/
						found = true;
						ArrayList<Element> resultList = new ArrayList<Element>();
						resultList.addAll(path);
						if (!hasError) { /*hier wird auf Fehler geprüft. Ist "hasError" noch auf false, 
							heisst dies, dass vorher noch kein Fehler gefunden wurde. Für eine strukturierte 
							Ausgabe wird dann als erstes in den Report geschrieben, dass der Test fehl schlug.*/
							host.appendLineToReport("Test failed with respect to <<fair-exchange>>.");
							AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.ERROR,
									"Test failed with respect to <<fair-exchange>>.");
							host.addResultMessage(analysisResultMessage);
						}
						host.appendLineToReport("Following path failed:");
						for (Element activity : resultList) {
							host.appendToReport("--->" + ((NamedElement) activity).getName());
						}
						host.appendToReport("\n");
						hasError = true;
					}
				}
			}
		}
	}
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost analysisHost) {
		if (analysisHost != null) {
			host = analysisHost;
		}
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Resource is null"));
			return false;
		}
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			elementWithFairExchange = (Package) currentModel.getContents().get(0);
			return fairExchangeAnalysis(elementWithFairExchange);
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
		}
	}
}