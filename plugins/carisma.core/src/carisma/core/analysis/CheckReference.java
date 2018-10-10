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
package carisma.core.analysis;

import java.util.ArrayList;
import java.util.List;

import carisma.core.checks.CheckParameter;
import carisma.core.checks.ParameterType;


/**
 * Refers to a check that is used in a certain analysis.
 * @author wenzel
 *
 */
public class CheckReference {

	/**
	 * ID of the referred check.
	 */
	private String checkID;
	
	/**
	 * Information whether the check is enabled. A check can be disabled to store it inside an analysis
	 * configuration, without executing it.
	 */
	private boolean enabled;
	
	/**
	 * Definition of the parameters given to the referred check.
	 */
	private List<CheckParameter> parameters;
	
	/**
	 * Creates a check reference.
	 * @param checkID the check Id
	 * @param enabled whether the check is enabled for the containing analysis.
	 */
	public CheckReference(final String checkID, final boolean enabled) {
		super();
		this.checkID = checkID;
		this.enabled = enabled;
		this.parameters = new ArrayList<>();
	}
	
	/**
	 * 
	 * @return String the checkId
	 */
	public final String getCheckID() {
		return this.checkID;
	}
	
	/**
	 * 
	 * @return boolean enabled
	 */
	public final boolean isEnabled() {
		return this.enabled;
	}
	
	/**
	 * 
	 * @param enabled enabled
	 */
	public final void setEnabled(final boolean enabled) {
		this.enabled = enabled;
	}
	
	/**
	 * List of parameters to be given to the referenced check.
	 * The list can be manipulated. 
	 * @return the parameters in the reference
	 */
	public final List<CheckParameter> getParameters() {
		return this.parameters;
	}

	/**
	 * @return List<CheckParameter> of required parameter, that are not set or are invalid.
	 */
	public final List<CheckParameter> getUnsetRequiredParameters() {
		List<CheckParameter> unsetParameters = new ArrayList<>();

		for (final CheckParameter param : this.getParameters()) {	
			if (!param.getDescriptor().isOptional() 
					&& !param.isQueryOnDemand() 
					&& !unsetParameters.contains(param)) {
				//FIXME: Float and Integer parameters are ignored here
				if (param.getDescriptor().getType().equals(ParameterType.STRING)) {
					StringParameter paramInstance = (StringParameter) param;
						if (paramInstance.getValue() == null) {
							unsetParameters.add(paramInstance);
						} else if ("".equals(paramInstance.getValue())) { 
							unsetParameters.add(paramInstance);
						}
				} else if (param.getDescriptor().getType().equals(ParameterType.INPUTFILE)) {
					InputFileParameter paramInstance = (InputFileParameter) param;
					if (paramInstance.getValue() == null) {
						unsetParameters.add(paramInstance);
					} else if (!paramInstance.getValue().exists()) { 
						unsetParameters.add(paramInstance);
					}
				} else if (param.getDescriptor().getType().equals(ParameterType.OUTPUTFILE)) {
					OutputFileParameter paramInstance = (OutputFileParameter) param;
					if (!paramInstance.isInsertedValueValid()) {
						unsetParameters.add(paramInstance);
					}
				} else if (param.getDescriptor().getType().equals(ParameterType.FOLDER)) {
					FolderParameter paramInstance = (FolderParameter) param;
					if (paramInstance.getValue() == null) {
						unsetParameters.add(paramInstance);
					} else if ("".equals(paramInstance.getValue().getAbsolutePath())) { 
						unsetParameters.add(paramInstance);
					}
				}
			}			
		}

		return unsetParameters;
	}
	
}
