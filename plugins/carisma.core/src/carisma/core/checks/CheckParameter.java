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

/**
 * Represents a configured parameter that is given to an analysis check.
 * It can be configured to be on demand. The parameter does not store a 
 * value, but the check framework has to ask the user when the parameter is
 * required.
 * @author wenzel
 *
 */
public abstract class CheckParameter {

	private CheckParameterDescriptor descriptor;
	private boolean queryOnDemand;
	
	public CheckParameter(CheckParameterDescriptor descriptor, boolean queryOnDemand) {
		super();
		this.descriptor = descriptor;
		this.queryOnDemand = queryOnDemand;
	}
	
	public CheckParameterDescriptor getDescriptor() {
		return this.descriptor;
	}

	public boolean isQueryOnDemand() {
		return this.queryOnDemand;
	}

	public void setQueryOnDemand(boolean queryOnDemand) {
		this.queryOnDemand = queryOnDemand;
	}
}
