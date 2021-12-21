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
package carisma.core.analysis.result;

import java.util.ArrayList;
import java.util.List;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * @author buerger
 * This class represents the results of a check.
 */
@XmlRootElement(name = "CheckResult")
@XmlType(propOrder = { "name","successful","results"})
public class CheckResult{

	/**
	 * Expresses whether the check has reported success.
	 */
	private boolean successful;

	/**
	 * The name of the check whose result is represented.
	 */
	private String name;

	private StatusType status = StatusType.INFO;

	/**
	 * List of result messages inside the check result.
	 */
	private final ArrayList<AnalysisResultMessage> results;

	private AnalysisResult parent;

	public CheckResult(){
		this.successful=false;
		this.name="";
		this.results=new ArrayList<>();
	}

	@XmlElement(name = "successful")
	public boolean isSuccessful() {
		return this.successful;
	}

	public void setSuccessful(final boolean successful) {
		this.successful = successful;
	}


	@XmlElement(name = "name")
	public String getName() {
		return this.name;
	}

	public void setName(final String name) {
		this.name = name;
	}


	//return Collections.unmodifiableList(results);
	@XmlElement(name = "results")
	public List<AnalysisResultMessage> getResults() {
		return this.results;
	}

	public void addResult(final AnalysisResultMessage result) {
		this.results.add(result);
		result.setParent(this);
		this.status = StatusType.max(this.status, result.getStatus());
	}

	void setParent(final AnalysisResult parent) {
		this.parent = parent;
	}

	public AnalysisResult getParent() {
		return this.parent;
	}

	public StatusType getStatus() {
		return this.status;
	}

}
