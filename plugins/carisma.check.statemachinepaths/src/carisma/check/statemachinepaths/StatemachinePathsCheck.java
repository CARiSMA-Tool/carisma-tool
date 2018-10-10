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
package carisma.check.statemachinepaths;

import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.StateMachine;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
// import carisma.modeltype.uml2.UMLHelper;
// import carisma.profile.umlchange.UMLchange;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.statemachine.StateMachinePaths;

/**
 * StatemachinePathsCheck-Class to manage the Class StateMachinePaths.
 * @author ---
 *
 */
public class StatemachinePathsCheck implements CarismaCheckWithID {
	
	private static final String CHECK_ID = "carisma.check.statemachinepaths";
	private static final String CHECK_NAME = "statemachinepathscheck";

	/**
	 * instance of Statemachinepaths to get all ways out of a state machine.
	 */
	private StateMachinePaths p = new StateMachinePaths();
	
	/**
	 * list to get all statemachines out of a model.
	 */
	private List<StateMachine> mach;
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			Package model = (Package) currentModel.getContents().get(0);
			this.mach = UMLHelper.getAllElementsOfType(model, StateMachine.class);
			host.appendLineToReport(this.mach.size() + " StateMachines found.");
			for (int i = 0; i < this.mach.size(); i++) {
				this.p.getPaths(this.mach.get(i), host, false);
			}
			return true;
		}
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