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

import java.io.File;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.result.AnalysisResultMessage;


/**
 * Interface for the host which performs an analysis. It can be called by analysis checks to get the model to be analyzed or to produce output. 
 * @author wenzel
 *
 */
public interface AnalysisHost {

	/**
	 * Adds a @AnalysisResultMessage to the analysis result.  
	 * @param detail AnalysisResultMessage
	 */
	void addResultMessage(AnalysisResultMessage detail);
	
	/**
	 * Appends some text to the analysis report.
	 * @param text the message text
	 */
	void appendToReport(String text);
	
	/**
	 * Appends some text to the analysis report and completes the line with an <pre>\n</pre>. 
	 * @param text the message text
	 */
	void appendLineToReport(String text);
	
	/**
	 * Returns the model that is to be analyzed.
	 * @return Resource
	 */
	Resource getAnalyzedModel();
	
	/**
	 * Returns the filename of the model used in the analysis.
	 * @return - filename
	 */
	String getCurrentModelFilename();
	
	/**
	 * Puts some data object into the register specified by registerName.
	 * @param registerName register name
	 * @param data data Object
	 * @throws RegisterInUseException 
	 */
	void putToRegister(String registerName, Object data) throws RegisterInUseException;
	
	/**
	 * Checks whether a register is in use.
	 * @param registerName register name
	 * @return boolean
	 */
	boolean isRegisterInUse(String registerName);
	
	/**
	 * Gets data from the register with name given in registerName.
	 * @param registerName register name
	 * @return Object
	 * @throws RegisterNotInUseException the exception
	 */
	Object getFromRegister(String registerName) throws RegisterNotInUseException;
	
	/**
	 * Removes the data from the register with name given in registerName.
	 * @param registerName register name
	 * @return Object
	 * @throws RegisterNotInUseException the exception
	 */
	Object removeFromRegister(String registerName) throws RegisterNotInUseException;
	
	/**
	 * Displays an error in the GUI.
	 * This method is used to display user errors. It is NOT for error detected during analysis.
	 * @param message the error message
	 */
	void displayError(String message);
	
	/**
	 * 
	 * @param file the file to be checked 
	 * @throws UserAbortedAnalysisException an exception due to user abortion
	 * @return the same file, if it already exist, or 
	 * 			 new file (with another name), after interacting with user
	 */
	File getFileToBeWritten(File file) throws UserAbortedAnalysisException;

	
}
