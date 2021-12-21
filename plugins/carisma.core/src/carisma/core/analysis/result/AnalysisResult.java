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

import carisma.core.analysis.Analysis;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;


/**
 * @author buerger
 * This class represents the results of an analysis. It consists of a number of CheckResult-objects.
 */

@XmlRootElement(name = "AnalysisResult")
@XmlType(propOrder = { "name","modelPath", "status", "timestamp", "checkResults", "report"})
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
	private final ArrayList<CheckResult> checkResults;

	/**
	 *
	 */
	private StringBuffer report;

	private String modelPath;

	/*
	 * No-arg constructor is just to keep JAXB from complaining
	 */
	@SuppressWarnings("unused")
	private AnalysisResult() {
		this.checkResults = new ArrayList<>();
		//throw new UnsupportedOperationException("No-arg constructor is just to keep JAXB from complaining");
	}

	/**
	 * @param analysis Analysis
	 */
	public AnalysisResult(final Analysis analysis){
		this.analysis = analysis;
		this.timestamp = "";
		this.status = AnalysisResultStatus.FAIL;
		this.checkResults = new ArrayList<>();
		this.report = new StringBuffer();
		this.modelPath = analysis.getModelFile().toString();//das
	}

	/**
	 * @return String name
	 */
	@XmlElement(name = "AnalyzedModel")
	public final String getName() {
		return this.name;
	}


	@XmlElement(name = "Filepath")
	public final String getModelPath(){
		return this.modelPath;
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
	@XmlElement(name = "Timestamp")
	public final String getTimestamp() {
		return this.timestamp;
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
	@XmlElement(name = "Status")
	public final AnalysisResultStatus getStatus(){
		return this.status;
	}


	/**
	 * @return a list of CheckResults
	 * should it be return Collections.unmodifiableList(checkResults);
	 */
	@XmlElement(name = "CheckResult")
	public final List<CheckResult> getCheckResults() {
		return this.checkResults;
	}


	/**
	 * @param text the message
	 */
	public final void appendToReport(final String text) {
		this.report.append(text);
	}

	/**
	 * @return String report
	 */
	@XmlElement(name = "ReportDump")
	public final String getReport() {
		return this.report.toString();
	}

	/**
	 * @return analysis
	 */
	public final Analysis getAnalysis() {
		return this.analysis;
	}
	/**
	 *
	 * @param result the CheckReuslt
	 */
	public final void addCheckResult(final CheckResult result) {
		this.checkResults.add(result);
		result.setParent(this);
	}

}
