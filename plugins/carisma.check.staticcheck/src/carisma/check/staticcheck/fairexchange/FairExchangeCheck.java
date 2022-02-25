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
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * analyzes an activity diagram with respect to fair exchange.
 * @author Klaus Rudack
 *
 */
public class FairExchangeCheck implements CarismaCheckWithID {
	
	public static final String CHECK_ID = "carisma.check.staticcheck.fairexchange";
	public static final String CHECK_NAME = "UMLsec fairexchange Check";

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
	private List<List<Element>> pathsList = new ArrayList<>();
	
	/**
	 * The given Stereotype fair-exchange.
	 */
	private Stereotype stereotype = null;
	
	/**
	 * AnalysisHost for report.
	 */
	private AnalysisHost dummyHost = new DummyHost(true);
	
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
			this.elementWithFairExchange = givenModel;
		if (givenModel == null) {
			AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.WARNING, "The given Model is null.");
			this.dummyHost.addResultMessage(analysisResultMessage);
			return false;
		}
		this.stereotype = this.elementWithFairExchange.getAppliedStereotype(fairName);
		if (this.stereotype == null) {
			List<Element> stereotypeList = UMLsecUtil.getStereotypedElements(givenModel, UMLsec.FAIR_EXCHANGE);
			if ((stereotypeList == null) || (stereotypeList.isEmpty())) {
				this.dummyHost.appendLineToReport("No stereotype <<fair-exchange>> applied.");
				AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.INFO,
						"No stereotype <<fair-exchange>> applied.");
				this.dummyHost.addResultMessage(analysisResultMessage);
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
				ActivityDiagramManager adm = new ActivityDiagramManager(givenModel, this.dummyHost);
				this.pathsList = adm.getAllPaths();
				if (this.pathsList.isEmpty()) {
					this.dummyHost.appendLineToReport("No possible way through this ActivityDiagramm.");
					AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.INFO,
							"No possible way through this ActivityDiagramm.");
					this.dummyHost.addResultMessage(analysisResultMessage);
				} else {
					startAnalysis(this.dummyHost);
				}
		}
		if (!this.hasError) {
			this.dummyHost.appendLineToReport("NoDownFlowCheck successfull with respect to <<fair exchange>>.");
			AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.INFO,
					"Test successfull with respect to <<fair-exchange>>.");
			this.dummyHost.addResultMessage(analysisResultMessage);
		}
		return !this.hasError;
	}
	
	/**
	 * checks if all tags of the Stereotype got content.
	 * @return true if all tags got content, false otherwise
	 */
	@SuppressWarnings("unchecked")
	private boolean checkStereotype() {
		if (((List<List<Element>>) this.elementWithFairExchange.getValue(this.stereotype, "start")).isEmpty()) {
			this.dummyHost.appendLineToReport("No start-Elements are defined.");
			this.dummyHost.appendLineToReport("NoDownFlowCheck failed with respect to fair exchange.");
			AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.WARNING,
					"No start-Elements are defined.");
			this.dummyHost.addResultMessage(analysisResultMessage);
			return false;
		}
		if (((List<List<Element>>) this.elementWithFairExchange.getValue(this.stereotype, "stop")).isEmpty()) {
			this.dummyHost.appendLineToReport("No stop-Elements are defined.");
			this.dummyHost.appendLineToReport("NoDownFlowCheck failed with respect to fair exchange.");
			AnalysisResultMessage analysisResultMessage = new AnalysisResultMessage(StatusType.ERROR,
					"No stop-Elements are defined.");
			this.dummyHost.addResultMessage(analysisResultMessage);
			return false;
		}
		return true;
	}
	
	/**
	 * analyzes the paths of the given ActivityDiagram.
	 * @param host AnalysisHost for report
	 */
	private void startAnalysis(final AnalysisHost host) {
		EObjectResolvingEList<?> startList = (EObjectResolvingEList<?>) this.elementWithFairExchange.getValue(this.stereotype, "start");
		EObjectResolvingEList<?> stopList = (EObjectResolvingEList<?>) this.elementWithFairExchange.getValue(this.stereotype, "stop");
		for (List<Element> path : this.pathsList) {
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
						ArrayList<Element> resultList = new ArrayList<>();
						resultList.addAll(path);
						if (!this.hasError) { /*hier wird auf Fehler gepr�ft. Ist "hasError" noch auf false, 
							heisst dies, dass vorher noch kein Fehler gefunden wurde. F�r eine strukturierte 
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
						this.hasError = true;
					}
				}
			}
		}
	}
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost analysisHost) {
		if (analysisHost != null) {
			this.dummyHost = analysisHost;
		}
		Resource currentModel = this.dummyHost.getAnalyzedModel();
		if (currentModel == null) {
			this.dummyHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Resource is null"));
			return false;
		}
		if (currentModel.getContents().isEmpty()) {
			this.dummyHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			this.elementWithFairExchange = (Package) currentModel.getContents().get(0);
			return fairExchangeAnalysis(this.elementWithFairExchange);
		}
		this.dummyHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
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