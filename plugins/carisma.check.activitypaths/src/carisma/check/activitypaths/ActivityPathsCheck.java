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
package carisma.check.activitypaths;


import java.util.Map;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;

/**
 * @author 
 *
 */
public class ActivityPathsCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.activitypaths";

	public static final String CHECK_NAME = "activitypaths";

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.appendToReport("No Model can be read");
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			Package model = (Package) currentModel.getContents().get(0);

			ActivityDiagramManager manager = new ActivityDiagramManager(model, host);
			host.appendToReport(manager.debugOutput());
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