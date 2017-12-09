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
package carisma.check.staticcheck.securedependency;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;

public class SecureDependenciesCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.staticcheck.securedependency";
	public static final String CHECK_NAME = "UMLsec secure dependency Check";

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost newHost) {
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
		if (currentModel.getContents().get(0) instanceof Package) {
			Package model = (Package) currentModel.getContents().get(0);
			SecureDependencyChecks sdc = new SecureDependencyChecks(host);
			int violations = sdc.checkSecureDependency(model);
			for (SecureDependencyViolation v : sdc.getViolations()) {
				String s = computeMessage(v);
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, s));
			}
			return violations == 0;
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
	}

	private static String computeMessage(SecureDependencyViolation v) {
		if (v.getDescription() != null) { // TODO: What is the purpose of this stuff???
			if (v.getDescription().startsWith("Dependency")) {
				String ns = v.getDependency().getName();
				if (ns == null || "".equals(ns)) {
					ns = v.getClient().getName() + "->" + v.getSupplier().getName();
				}
				return v.getDescription().replace("Dependency", "Dependency '" + ns + "'");
			}
			if (v.getDescription().startsWith("Supplier")) {
				String ns = v.getSupplier().getName();
				if (ns == null) {
					ns = "";
				}
				return v.getDescription().replace("Supplier", "Supplier '" + ns + "'");
			}
			if (v.getDescription().startsWith("Client")) {
				String ns = v.getClient().getName();
				if (ns == null) {
					ns = "";
				}
				return v.getDescription().replace("Client", "Client '" + ns + "'");
			}
			if (v.getDescription().contains("And client")) {
				String ns = v.getClient().getName();
				if (ns == null) {
					ns = "";
				}
				return v.getDescription().replace("And client", "And client '" + ns + "'");
			}
		}
		return v.getDescription();
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
