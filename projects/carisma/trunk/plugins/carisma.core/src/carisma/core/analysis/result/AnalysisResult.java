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
import java.util.Collections;
import java.util.List;

import carisma.core.analysis.Analysis;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * @author buerger
 * This class represents the results of an analysis. It consists of a number of CheckResult-objects.
 */

@XmlRootElement(name = "AnalysisResult")
@XmlType(propOrder={"status", "timestamp", "CheckResult",  "ReportDump" })

public class AnalysisResult {
	/**
	 * 
	 */
	private Analysis analysis;
	/**
	 * 
	 */
	private String name;
	
	/**
	 * String identifying a timestamp when the analysis has been started.
	 */
	private String timestamp;
	/**
	 * Status of AnalysisResult.
	 */
	private AnalysisResultStatus status;
	/**
	 * 
	 */
	private ArrayList<CheckResult> checkResults;
	/**
	 * 
	 */
	private StringBuffer report;

	/*
	 * No-arg constructor is just to keep JAXB from complaining
	 */
	private AnalysisResult() {
	    throw new UnsupportedOperationException("No-arg constructor is just to keep JAXB from complaining");
	}
	
	/**
	 * @param analysis Analysis
	 */
	public AnalysisResult(final Analysis analysis){
		this.analysis = analysis;
		timestamp = "";
		status = AnalysisResultStatus.FAIL;
		checkResults = new ArrayList<CheckResult>();
		report = new StringBuffer();
	}

	/**
	 * @return String name
	 */
	public final String getName() {
		return name;
	}

	/**
	 * @param name the name
	 */
	public final void setName(final String name) {
		this.name = name;
	}

	/**
	 * @return String timestamp
	 */
	public final String getTimestamp() {
		return timestamp;
	}
	
	/**
	 * @param timestamp timestamp
	 */
	public final void setTimestamp(final String timestamp) {
		this.timestamp = timestamp;
	}
	
	/**
	 * @param status 
	 */
	public final void setStatus(final AnalysisResultStatus status) {
		this.status = status;
	}
	
	/**
	 * @return status
	 */
	@XmlElement
	public final AnalysisResultStatus getStatus(){
		return status;
	}
	
	/**
	 * @return a list ot CheckResults
	 */
	@XmlElement(name = "CheckResult")
	public final List<CheckResult> getCheckResults() {
		return Collections.unmodifiableList(checkResults);
	}
	
	/**
	 * @param text the message
	 */
	public final void appendToReport(final String text) {
		report.append(text);
	}
	
	/**
	 * @return String report
	 */
	@XmlElement(name = "ReportDump")
	public final String getReport() {
		return report.toString();
	}
	
	/**
	 * @return analysis
	 */
	public final Analysis getAnalysis() {
		return analysis;
	}
	/**
	 * 
	 * @param result the CheckReuslt
	 */
	public final void addCheckResult(final CheckResult result) {
		checkResults.add(result);
		result.setParent(this);
	}
}
	