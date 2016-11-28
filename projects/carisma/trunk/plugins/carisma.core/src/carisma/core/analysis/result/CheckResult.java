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
//import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

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
	private ArrayList<AnalysisResultMessage> results;
	
	private AnalysisResult parent;

	public CheckResult(){
		this.successful=false;
		this.name="";
		this.results=new ArrayList<AnalysisResultMessage>();
	}
	
	@XmlElement(name = "sucessful")
	public boolean isSuccessful() {
		return this.successful;
	}

	public void setSuccessful(boolean successful) {
		this.successful = successful;
	}
	
	
	@XmlElement(name = "name")
	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	
	//return Collections.unmodifiableList(results);
	@XmlElement(name = "results")
	public List<AnalysisResultMessage> getResults() {
		return this.results;
	}
	
	public void addResult(AnalysisResultMessage result) {
		this.results.add(result);
		result.setParent(this);
		this.status = StatusType.max(this.status, result.getStatus());
	}

	void setParent(AnalysisResult parent) {
		this.parent = parent;
	}

	public AnalysisResult getParent() {
		return this.parent;
	}
	
	public StatusType getStatus() {
		return this.status;
	}

}
