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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


/**
 * This class will simulate an analysis host.
 * @author Marcel Michel
 *
 */
public class DebugHost implements AnalysisHost {

	/**
	 * The stored resultMessages.
	 */
	private List<AnalysisResultMessage> resultMessages;
	
	/**
	 * The stored reportMessages.
	 */
	private List<String> reportMessages;
	
	/**
	 * Flag to enable SystemOutput by using System.out.println.
	 */
	private boolean enableSystemOutput;
	
	/**
	 * The analyzed model.
	 */
	private Resource analyzedModel;
	
	/**
	 * Constructor.
	 */
	public DebugHost() {
		this.reportMessages = new ArrayList<>();
		this.resultMessages = new ArrayList<>();
		this.enableSystemOutput = false;
	}
	
	/**
	 * Constructor.
	 * @param enableSystemOutput Enables the system output by using System.out.println 
	 */
	public DebugHost(final boolean enableSystemOutput) {
		this.reportMessages = new ArrayList<>();
		this.resultMessages = new ArrayList<>();
		this.enableSystemOutput = enableSystemOutput;
	}
	
	/**
	 * Adds an analysis result message to the report.
	 * @param detail The detailed description
	 */
	@Override
	public final void addResultMessage(final AnalysisResultMessage detail) {
		this.resultMessages.add(detail);
		if (this.enableSystemOutput) {
			Logger.log(LogLevel.INFO, (this.getClass().getName().toUpperCase(Locale.ENGLISH) + ": " + detail.getText()));
		}
	}
	
	/**
	 * Returns the collected report Messages as a list.
	 * @return reportMessages 
	 */
	public final List<String> getReportMessages() {
		return this.reportMessages;
	}

	/**
	 * Appends a line to the report.
	 * @param text Line which should be appended to the report
	 */
	@Override
	public final void appendToReport(final String text) {
		this.reportMessages.add(text);
		if (this.enableSystemOutput) {
		    Logger.log(LogLevel.INFO, (this.getClass().getName().toUpperCase(Locale.ENGLISH) + ": " + text));
		}
	}

	/**
	 * Appends a line to the report.
	 * @param text Line which should be appended to the report
	 */
	@Override
	public final void appendLineToReport(final String text) {
		this.reportMessages.add(text);
		if (this.enableSystemOutput) {
		    Logger.log(LogLevel.INFO, (this.getClass().getName().toUpperCase(Locale.ENGLISH) + ": " + text));
		}
	}
	
	/**
	 * Returns the collected result messages.
	 * @return resultMessages as a list
	 */
	public final List<AnalysisResultMessage> getResultMessages() {
		return this.resultMessages;
	}
	
	/**
	 * Getter for the attrivute analyzed model.
	 * @return The analyzed model
	 */
	@Override
	public final Resource getAnalyzedModel() {
		return this.analyzedModel;
	}
	
	/**
	 * Sets the analyzed model.
	 * @param model The model
	 */
	public final void setAnalyzedModel(Resource model) {
		this.analyzedModel = model;
	}

	/**
	 * Dummy.
	 * @return null
	 */
	@Override
	public final String getCurrentModelFilename() {
		return null;
	}

	/**
	 * Dummy.
	 * @param registerName 
	 * @param data  
	 * @throws RegisterInUseException 
	 */
	@Override
	public void putToRegister(final String registerName, final Object data)
			throws RegisterInUseException {
		// Do nothing, only a dummy
	}

	/**
	 * Dummy.
	 * @param registerName 
	 * @return false
	 */
	@Override
	public final boolean isRegisterInUse(final String registerName) {
		return false;
	}

	/**
	 * Dummy.
	 * @param registerName 
	 * @throws RegisterNotInUseException 
	 * @return null
	 */
	@Override
	public final Object getFromRegister(final String registerName)
			throws RegisterNotInUseException {
		return null;
	}
	
	/**
	 * Dummy.
	 * @param registerName 
	 * @throws RegisterNotInUseException 
	 * @return null
	 */
	@Override
	public final Object removeFromRegister(final String registerName)
			throws RegisterNotInUseException {
		return null;
	}

	@Override
	public void displayError(String message) {
		// TODO Auto-generated method stub
	    Logger.log(LogLevel.INFO, message);
	}

	@Override
	public File getFileToBeWritten(File file)
			throws UserAbortedAnalysisException {
		// TODO Auto-generated method stub
		return file;
	}

}
