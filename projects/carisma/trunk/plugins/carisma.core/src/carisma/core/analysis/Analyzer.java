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
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.Carisma;
import carisma.core.analysis.result.AnalysisResult;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.AnalysisResultStatus;
import carisma.core.analysis.result.CheckResult;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.util.Utils;

/**
 *
 */
public class Analyzer implements AnalysisHost {
	/**
	 * 
	 */
	private AnalysisResult result = null;
	/**
	 * 
	 */
	private CheckResult currentCheckResult = null;
	/**
	 * 
	 */
	private Resource currentModel = null;
	/**
	 * 
	 */
	private String currentModelFilename = "";
	/**
	 * 
	 */
	private Map<String, Object> register = null;
	/**
	 * 
	 */
	private Map<CheckReference, CheckDescriptor> checkDescriptorMapping = null;
	/**
	 * 
	 */
	private CheckDescriptor currentCheckDescriptor = null;
	/**
	 * 
	 */
	private int lastReportSource = 0; //0 = ?,  1 = message,  2 = append
	
	/**
	 * 
	 */
	private UIConnector uiConnector = null;


	/**
	 * @return String the current model file name
	 */
	public final String getCurrentModelFilename() {
		return currentModelFilename;
	}
	
	/**
	 * Performs the given analysis.
	 * @param analysis analysis
	 */
	public final void runAnalysis(final Analysis analysis, UIConnector connector) {
		// initialize
		this.register = new HashMap<String, Object>();
		this.result = new AnalysisResult(analysis);
		this.result.setName(analysis.getName() + " (" + analysis.getModelType() + ")");
		this.result.setTimestamp(Utils.getISOTimestamp());
		uiConnector = connector;
		Carisma.getInstance().getAnalysisResults().add(result);
		uiConnector.updateView();
		
		AnalysisResultStatus status = AnalysisResultStatus.RUNNING;
		this.result.setStatus(AnalysisResultStatus.RUNNING);
		
		Map<CheckDescriptor, List<String>> failedCheckConditions = checkConditions(analysis);
		if (!failedCheckConditions.isEmpty()) {
			this.result.setStatus(AnalysisResultStatus.FAIL);
			uiConnector.updateView();
			Logger.log(LogLevel.ERROR, "Analysis not executed due to missing requirements!");
			for (Entry<CheckDescriptor, List<String>> entry : failedCheckConditions.entrySet()) {
				Logger.log(LogLevel.ERROR, "Check '" + entry.getKey().getName() + "' misses:");
				for (String preconditionName : entry.getValue()) {
					Logger.log(LogLevel.ERROR, "  " + preconditionName);
				}
			}
			//TODO error dialog
			return;
		}
		
		// load model
		File file = Carisma.getInstance().getModelManager().getFile(analysis.getIFile());
		try {
			currentModel = Carisma.getInstance().getModelManager().loadModel(file, analysis.getModelType());
		} catch (IOException e) {
			currentCheckResult = new CheckResult();
			currentCheckResult.setName("ERROR");
			this.result.addCheckResult(currentCheckResult);
			addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
					"Model file not found. Please check your analysis configuration."));
			this.result.setStatus(AnalysisResultStatus.FAIL);
			uiConnector.updateView();
			Logger.log(LogLevel.ERROR, "Unable to load model '" + file.getAbsolutePath() + "'", e);
			String answer[] = {"Ok"};
			uiConnector.sendMessage("Error", "Unable to load model '" + file.getAbsolutePath() + "'", StatusType.ERROR, answer, 0);
			return;
		}
		
		currentModelFilename = analysis.getIFile().getName();
		
		// run analysis
		for (CheckReference checkReference : analysis.getChecks()) {
			if (!checkReference.isEnabled()) {
				continue;
			}
			CheckDescriptor checkDescriptor = checkDescriptorMapping.get(checkReference);
			if (checkDescriptor == null) {
				this.result.setStatus(AnalysisResultStatus.FAIL);
				uiConnector.updateView();
				Logger.log(LogLevel.ERROR, "Descriptor of check '" + checkReference.getCheckID() + "' not found!");
				return;
			}
			
			currentCheckDescriptor = checkDescriptor;
			
			currentCheckResult = new CheckResult();
			currentCheckResult.setName(checkDescriptor.getName());
			this.result.addCheckResult(currentCheckResult);
			uiConnector.updateView();
			this.lastReportSource = 0;
			this.internalAppendLineToReport("------------------------------------------------------------------------------------");
			this.internalAppendLineToReport("------------------------------------------------------------------------------------");
			this.internalAppendLineToReport("Running check : " + currentCheckResult.getName());

			// check preconditions
			{
				List<String> missingPreconditions = checkPreconditions(checkDescriptor);
				if (!missingPreconditions.isEmpty()) {
					internalAppendLineToReport("");
					internalAppendLineToReport("Preconditions violated! Registry content missing:");
					for (String f : missingPreconditions) {
						internalAppendLineToReport("  " + f);
					}
					addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
							"Preconditions violated! " + missingPreconditions.size() + "registry content(s) missing - view report for more information."));
					this.result.setStatus(AnalysisResultStatus.FAIL);
					uiConnector.updateView();
					return;
				}
			}
			// check parameters and create copy for performing the check
			List<CheckParameterDescriptor> paramDescriptors = new ArrayList<CheckParameterDescriptor>(checkDescriptor.getParameters());
			Map<String, CheckParameter> checkParameters = new HashMap<String, CheckParameter>(checkReference.getParameters().size());
			
			for (CheckParameter param : checkReference.getParameters()) {
				CheckParameter tmpPara = null;
				if (param instanceof BooleanParameter) {
					tmpPara = new BooleanParameter(param.getDescriptor(), ((BooleanParameter) param).getValue());
				} else if (param instanceof StringParameter) {
					tmpPara = new StringParameter(param.getDescriptor(), ((StringParameter) param).getValue());
				} else if (param instanceof IntegerParameter) {
					tmpPara = new IntegerParameter(param.getDescriptor(), ((IntegerParameter) param).getValue());
				} else if (param instanceof FloatParameter) {
					tmpPara = new FloatParameter(param.getDescriptor(), ((FloatParameter) param).getValue());
				} else if (param instanceof InputFileParameter) {
					tmpPara = new InputFileParameter(param.getDescriptor(), ((InputFileParameter) param).getValue());
				} else if (param instanceof FolderParameter) {
					tmpPara = new FolderParameter(param.getDescriptor(), ((FolderParameter) param).getValue());
				} else if (param instanceof OutputFileParameter) {
					tmpPara = new OutputFileParameter(param.getDescriptor(), ((OutputFileParameter) param).getValue());
				} else {
					Logger.log(LogLevel.ERROR, "Unknown parameter type: " + param.getClass().getName());
				}
//FIXME: DW-Aufraeumen
				if (tmpPara != null) {
					if (param.isQueryOnDemand()) {
						tmpPara = uiConnector.askParameter(checkDescriptor, tmpPara);
					} else {
						if (param instanceof OutputFileParameter) {
							try { 
								File newFile = getFileToBeWritten(((OutputFileParameter) param).getValue());
								tmpPara = new OutputFileParameter(param.getDescriptor(), newFile);
								uiConnector.updateView();
							} catch (UserAbortedAnalysisException e) {
								Logger.log(LogLevel.ERROR, "Abort due to user: " + checkReference.getCheckID() 
										+ " at parameter of type " + param.getDescriptor().getType()
										+ " (" + ((OutputFileParameter) param).getValue() + ")");
								this.result.setStatus(AnalysisResultStatus.FAIL);
								uiConnector.updateView();
								return;
							}
						}
					}
					if (tmpPara instanceof FolderParameter) {
						File checkFolder = ((FolderParameter) tmpPara).getValue();
						if (!checkFolder.canRead()) {
							addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
									"Folder referenced by parameter '" + param.getDescriptor().getName() + "' does not exist!"));
							Logger.log(LogLevel.ERROR, "Folder referenced by parameter '" + param.getDescriptor().getName() + "' does not exist!");
							this.result.setStatus(AnalysisResultStatus.FAIL);
							uiConnector.updateView();
							return;
						}
					}
					checkParameters.put(tmpPara.getDescriptor().getID(), tmpPara);
					tmpPara = null;
				}

				CheckParameterDescriptor pd = param.getDescriptor();
				if (!paramDescriptors.contains(pd)) {
					Logger.log(LogLevel.ERROR, "Unknown parameter: " + pd.getName() + " (" + pd.getID() + ")");
				} else {
					paramDescriptors.remove(pd);
				}
			}
			if (!paramDescriptors.isEmpty()) {
				StringBuffer errorString = new StringBuffer();
				for (CheckParameterDescriptor pd : paramDescriptors) {
					errorString.append(", " + pd.getName() + " (" + pd.getID() + ")");
				}
				Logger.log(LogLevel.ERROR, "Parameters undefined: " + errorString.toString());
			}
			
			CarismaCheck check = Carisma.getInstance().getCheckRegistry().getCheck(checkDescriptor);

			if (!(checkParameters.size() < 1)) {
				this.internalAppendLineToReport("Parameters:");
			}
				for (CheckParameter param : checkParameters.values()) {
						if (param instanceof BooleanParameter) {
							BooleanParameter para = (BooleanParameter) param;
							this.internalAppendLineToReport(para.getDescriptor().getName() + " : " + para.getValue());
						} else if (param instanceof StringParameter) {
							StringParameter para = (StringParameter) param;
							this.internalAppendLineToReport(para.getDescriptor().getName() + " : " + para.getValue());
						} else if (param instanceof IntegerParameter) {
							IntegerParameter para = (IntegerParameter) param;
							this.internalAppendLineToReport(para.getDescriptor().getName() + " : " + para.getValue());
						} else if (param instanceof InputFileParameter) {
							InputFileParameter para = (InputFileParameter) param;
							this.internalAppendLineToReport(para.getDescriptor().getName() + " : " + para.getValue());
						} else if (param instanceof FolderParameter) {
							FolderParameter para = (FolderParameter) param;
							this.internalAppendLineToReport(para.getDescriptor().getName() + " : " + para.getValue());
						} else if (param instanceof OutputFileParameter) {
							OutputFileParameter para = (OutputFileParameter) param;
							this.internalAppendLineToReport(para.getDescriptor().getName() + " : " + para.getValue());
						} else {
							Logger.log(LogLevel.ERROR, "Unknown parameter type: " + param.getClass().getName());
						}
			}
			this.internalAppendLineToReport("------------------------------------------------------------------------------------");
			this.lastReportSource = 0;
			boolean checkSuccess;
	
			try {
				checkSuccess = check.perform(checkParameters, this);
			} catch (Exception e) {
				internalAppendLineToReport("");
				internalAppendLineToReport("A java error occurred while performing check.");
				internalAppendLineToReport("Exception: " + e.getClass().getSimpleName() + " - " + e.getMessage());
				
				internalAppendLineToReport("");
				internalAppendLineToReport("Stacktrace:");
				for (StackTraceElement traceElement : e.getStackTrace()) {	
					internalAppendLineToReport(traceElement.toString());
				}
				Logger.log(LogLevel.ERROR, e.getMessage(), e);

				addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
						"A java error occurred while performing check - view report for more information."));
				checkSuccess = false;
			}
			
			// check post conditions
			if (checkSuccess) {
				List<String> failed = checkPostconditions(checkDescriptor);
				if (!failed.isEmpty()) {
					internalAppendLineToReport("");
					internalAppendLineToReport("Postconditions violated! Check does not provide registry content:");
					for (String f : failed) {
						internalAppendLineToReport("  " + f);
					}
					addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
							"Postconditions violated! " + failed.size() + "registry content(s) missing - view report for more information."));
					checkSuccess = false;
				}
			}
			currentCheckResult.setSuccessful(checkSuccess);
			if (!checkSuccess) {
				status = AnalysisResultStatus.FAIL;
			}
		}
		if (status.equals(AnalysisResultStatus.RUNNING)) {
			status = AnalysisResultStatus.SUCCESS;
		}
		this.result.setStatus(status);
		uiConnector.updateView();
	}
	
	/**
	 * 
	 * @param checkDescriptor the Check descriptor
	 * @return ListString failed
	 */
	private List<String> checkPreconditions(final CheckDescriptor checkDescriptor) {
		ArrayList<String> failed = new ArrayList<String>();
		for (String requiredKey : checkDescriptor.getRequiredKeys()) {
			if (!isRegisterInUse(requiredKey)) {
				failed.add(requiredKey);
			}
		}
		return failed;
	}
	
	/**
	 * 
	 * @param checkDescriptor the Check descriptor
	 * @return List<String> failed
	 */
	private List<String> checkPostconditions(final CheckDescriptor checkDescriptor) {
		ArrayList<String> failed = new ArrayList<String>();
		for (String providedKey : checkDescriptor.getProvidedKeys()) {
			if (!isRegisterInUse(providedKey)) {
				failed.add(providedKey);
			}
		}
		return failed;
	}
	
	/**
	 * 
	 * @param analysis the Analysis
	 * @return Map<CheckDescrpitor, List<String> failed
	 */
	private Map<CheckDescriptor, List<String>> checkConditions(final Analysis analysis) {
		Map<CheckDescriptor, List<String>> failedChecks = new HashMap<CheckDescriptor, List<String>>();
		this.checkDescriptorMapping = new HashMap<CheckReference, CheckDescriptor>();
		List<String> simulatedRegistry = new ArrayList<String>();
		for (CheckReference check : analysis.getChecks()) {
			if(check.isEnabled()){
				List<String> missingConditions = new ArrayList<String>();
				CheckDescriptor checkDescriptor = Carisma.getInstance().getCheckRegistry().getCheckDescriptor(check.getCheckID());
				this.checkDescriptorMapping.put(check, checkDescriptor);
				for (String precondition : checkDescriptor.getRequiredKeys()) {
					if (!simulatedRegistry.contains(precondition) && !missingConditions.contains(precondition)) {
						missingConditions.add(precondition);
					}
				}
				if (!missingConditions.isEmpty()) {
					failedChecks.put(checkDescriptor, missingConditions);
				}
				for (String providedKey : checkDescriptor.getProvidedKeys()) {
					if (!simulatedRegistry.contains(providedKey)) {
						simulatedRegistry.add(providedKey);
					}
				}
			}
		}
		return failedChecks;
	}
	

	@Override
	public final void addResultMessage(final AnalysisResultMessage detail) {
		if (lastReportSource == 2) {
			internalAppendToReport("\n");
		}
		internalAppendToReport(detail.getStatus().toString() + ": " + detail.getText());
		if (detail.getModelElement() != null) {
			internalAppendToReport(" (element: '" + detail.getModelElement() + "'");
		}
		if (detail.getAdditionalInformation() != null) {
			internalAppendToReport(" " + detail.getAdditionalInformation());
		}
		internalAppendLineToReport("");
		currentCheckResult.addResult(detail);
		uiConnector.updateView();
		lastReportSource = 1;
	}
	
	/**
	 * 
	 * @param text the message text
	 */
	private void internalAppendToReport(final String text) {
		Logger.log(LogLevel.DEBUG, "Report: " + text, 1);
		this.result.appendToReport(text);
	}

	/**
	 * 
	 * @param text the message text
	 */
	public final void internalAppendLineToReport(final String text) {
		Logger.log(LogLevel.DEBUG, "Report: " + text, 1);
		this.result.appendToReport(text + "\n");
	}

	/**
	 * @param text the message text
	 */
	@Override
	public final void appendToReport(final String text) {
		if (lastReportSource == 1) {
			internalAppendToReport("\n");
		}
		internalAppendToReport(text);
		lastReportSource = 2;
	}

	/**
	 * @param text the message text
	 */
	@Override
	public final void appendLineToReport(final String text) {
		if (lastReportSource == 1) {
			internalAppendToReport("\n");
		}
		Logger.log(LogLevel.DEBUG, "Report: " + text, 1);
		this.result.appendToReport(text + "\n");
		lastReportSource = 2;
	}

	/**
	 * @return Resource the current model
	 */
	@Override
	public final Resource getAnalyzedModel() {
		return currentModel;
	}

	/**
	 * @param registerName registerName
	 * @param data data Object
	 * @throws RegisterInUseException the exception
	 */
	@Override
	public final void putToRegister(final String registerName, final Object data) throws RegisterInUseException {
		if (isRegisterInUse(registerName)) {
			throw new RegisterInUseException(registerName);
		}
		this.register.put(registerName, data);
	}

	/**
	 * @param registerName the register name7
	 * @return boolean 
	 */
	@Override
	public final boolean isRegisterInUse(final String registerName) {
		return this.register.containsKey(registerName);
	}

	/**
	 * @param registerName the register name
	 * @throws RegisterNotInUseException the exception
	 * @return Object
	 */
	@Override
	public final Object getFromRegister(final String registerName) throws RegisterNotInUseException {
		if (!isRegisterInUse(registerName)) {
			throw new RegisterNotInUseException(registerName);
		}
		return this.register.get(registerName);
	}

	/**
	 * @param registerName the register name
	 * @throws RegisterNotInUseException the exception
	 * @return Object
	 */
	@Override
	public final Object removeFromRegister(final String registerName) throws RegisterNotInUseException {
		if (!isRegisterInUse(registerName)) {
			throw new RegisterNotInUseException(registerName);
		}
		return this.register.remove(registerName);
	}

	@Override
	public final void displayError(final String message) {
		uiConnector.sendMessage("Error", 
				currentCheckDescriptor.getName() + ": " + message,
				StatusType.ERROR,
				new String[]{"OK"}, 0);
	}

	/**
	 * @param file the file to be checked
	 * @return if it doesn't exist return the same file back, otherwise 
	 * 			ask to overwrite or create new file
	 * @throws UserAbortedAnalysisException 
	 */
	@Override
	public final File getFileToBeWritten(final File file) throws UserAbortedAnalysisException {
		if (file == null || !file.exists()) {
			return file;
		}
				
		int answer = uiConnector.sendMessage("", 
				"File " + file + " already exists. Do you want to overwrite it?", 
				StatusType.INFO, 
				new String[]{"OK", "New File", "Cancel"}, 
				0);
		switch (answer) {
		case 0: 
			return file;
		case 1:
			String filepath = Utils.incrementFileNameIfNecessary(file.getAbsolutePath());
			File tmp = new File(filepath);
			if (!tmp.getName().equals(file.getName())) {
				uiConnector.sendMessage("File Name", 
						"Using file '" + tmp.getAbsolutePath() + "' for output.",
						StatusType.INFO,
						new String[] {"OK"}, 0);
			}
			return new File(filepath);			
			/*FileDialog fDialog = new FileDialog(shell, SWT.SAVE);
			fDialog.setFileName(file.getPath());
			fDialog.setOverwrite(true);
			String selection = fDialog.open();
			if (selection != null && !selection.isEmpty()) { 
				return new File(selection);
			}*/				
		default:
			break;
		}
		
		throw new UserAbortedAnalysisException();
	}
}
