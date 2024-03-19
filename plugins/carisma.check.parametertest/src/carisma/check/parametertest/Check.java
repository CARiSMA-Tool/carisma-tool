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
package carisma.check.parametertest;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.Map;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.FloatParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import java.util.logging.Logger;

public class Check implements CarismaCheck {
	
	private static final Logger logger = Logger.getLogger(Check.class.getName());
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		
		// String
		StringParameter stringParameter = (StringParameter)parameters.get("carisma.check.parametertest.string");
		String teststring = stringParameter.getValue();
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO,"Test string: '" + teststring + "'"));
		
		// Integer
		IntegerParameter integerParameter = (IntegerParameter)parameters.get("carisma.check.parametertest.integer");
		int testinteger = integerParameter.getValue();
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Test integer: " + testinteger));
		
		// Float
		FloatParameter floatParameter = (FloatParameter)parameters.get("carisma.check.parametertest.float");
		float testfloat = floatParameter.getValue();
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Test float: " + testfloat));
		
		// Boolean
		BooleanParameter booleanParameter = (BooleanParameter)parameters.get("carisma.check.parametertest.boolean");
		boolean testboolean = booleanParameter.getValue();
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Test boolean: " + testboolean));
		
		// InputFile
		InputFileParameter inputFileParameter = (InputFileParameter)parameters.get("carisma.check.parametertest.inputfile");
		if (inputFileParameter == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Test file: InputFileParameter = null!"));
			return false;
		}
		try {
			File testinputfile = inputFileParameter.getValue();
			if (!testinputfile.canRead()) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Test file: Can�t read test file!"));
			} else {
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Test file: " + testinputfile.getAbsolutePath()));
			}
		} catch (Exception e) {
			logger.warning("Error message: " + e.getMessage());
		}
		
		// Folder
		FolderParameter folderParameter = (FolderParameter)parameters.get("carisma.check.parametertest.folder");
		if (folderParameter == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Test folder: FolderParameter = null!"));
			return false;
		}
		try {
			File testfolder = folderParameter.getValue();
			if (!testfolder.canRead()) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Test folder: Can�t read test folder!"));
			} else {
				if (!testfolder.isDirectory()) {
					host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Test folder: This is not a folder!"));
				} else {
					host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Test folder: " + testfolder.getAbsolutePath()));
				}
			}
		} catch (Exception e) {
			logger.warning("Error message: " + e.getMessage());
		}
		
		// OutputFile
		OutputFileParameter outputFileParameter = (OutputFileParameter)parameters.get("carisma.check.parametertest.outputfile");
		if (outputFileParameter == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Test output file: OutputFileParameter = null!"));
			return false;
		}
		try {
			File testoutputfile = outputFileParameter.getValue();
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Test output file: " + testoutputfile.getAbsolutePath()));
			
			try (BufferedWriter bw = new BufferedWriter(new FileWriter(testoutputfile))) {
				bw.write("To test all parameters, the check will write them into this file:");
				bw.newLine();
				bw.write("StringParameter: " + stringParameter.getValue());
				bw.newLine();
				bw.write("IntegerParameter: " + integerParameter.getValue());
				bw.newLine();
				bw.write("FloatParameter: " + floatParameter.getValue());
				bw.newLine();
				bw.write("InputFileParameter: " + inputFileParameter.getValue().getAbsolutePath());
				bw.newLine();
				bw.write("BooleanParameter: " + booleanParameter.getValue());
				bw.newLine();
				bw.write("FolderParameter: " + folderParameter.getValue().getAbsolutePath());
				bw.newLine();
				bw.write("OutputFileParameter: " + outputFileParameter.getValue().getAbsolutePath());
				bw.close();
			}
		} catch (Exception e) {
			logger.warning("Error message: " + e.getMessage());
		}
		try {
			host.putToRegister("carisma.check.parametertest.post", "this is a postcondition entry to the register");
		} catch (RegisterInUseException riue) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Can't write to register 'carisma.check.parametertest.post' - it is already in use."));
		}
		
		return true;
	}

}
