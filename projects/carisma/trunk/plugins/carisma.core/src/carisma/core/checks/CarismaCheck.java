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
package carisma.core.checks;

import java.util.Map;

import carisma.core.analysis.AnalysisHost;


/**
 * Interface for analysis checks.
 * @author wenzel
 *
 */
public interface CarismaCheck {

	/**
	 * Performs the execution of the check
	 * @param parameters
	 * @param host
	 * @return true, if the analysis result is "successful"
	 */
	boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host);
	
}
