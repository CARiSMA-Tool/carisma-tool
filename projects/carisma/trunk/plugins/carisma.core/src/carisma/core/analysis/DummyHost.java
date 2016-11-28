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


public class DummyHost implements AnalysisHost {
	
	private boolean toSystemHost = false;
		
	public DummyHost(boolean printToSystemOut) {
		this.toSystemHost = printToSystemOut;
	}
	
	@Override
	public void addResultMessage(AnalysisResultMessage resultMessage) {
		if (this.toSystemHost) {
			System.out.println(resultMessage.getText());
		}
	}

	@Override
	public void appendToReport(String text) {
		if (this.toSystemHost) {
			System.out.print(text);
		}
	}

	@Override
	public void appendLineToReport(String text) {
		if (this.toSystemHost) {
			System.out.println(text);
		}
	}

	@Override
	public Resource getAnalyzedModel() {
		return null;
	}
	
	@Override
	public String getCurrentModelFilename() {
		return "";
	}

	@Override
	public void putToRegister(String registerName, Object data) throws RegisterInUseException {
	}

	@Override
	public boolean isRegisterInUse(String registerName) {
		return false;
	}

	@Override
	public Object getFromRegister(String registerName) throws RegisterNotInUseException {
		return null;
	}

	@Override
	public Object removeFromRegister(String registerName) throws RegisterNotInUseException {
		return null;
	}

	@Override
	public void displayError(String message) {
		System.err.println("Error: "+message);
		
	}

	@Override
	public File getFileToBeWritten(File file) {
		// TODO Auto-generated method stub
		return null;
	}
}
