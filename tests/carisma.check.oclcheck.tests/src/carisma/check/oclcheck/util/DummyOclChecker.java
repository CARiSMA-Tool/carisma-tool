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

package carisma.check.oclcheck.util;

import java.util.Map;

import org.eclipse.emf.ecore.EClass;

import carisma.check.oclcheck.AbstractOclChecker;
import carisma.core.analysis.AnalysisHost;
import carisma.core.checks.CheckParameter;


/**
 * Test class.
 * @author Marcel Michel
 * 
 */
public class DummyOclChecker extends AbstractOclChecker {

	// Not registered at carisma.carismacheck
	private static final String CHECK_ID = null;
	private static final String CHECK_NAME = null;
	
	private String oclStatement = "";
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters,
			AnalysisHost host) {
		return false;
	}

	public final void setOclStatement(String str) {
		this.oclStatement = str;
	}
	
	@Override
	protected String getOclStatement() {
		return this.oclStatement;
	}

	@Override
	protected EClass getOclContext() {
		return null;
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
