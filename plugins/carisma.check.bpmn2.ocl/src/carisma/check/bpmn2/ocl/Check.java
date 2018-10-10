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
package carisma.check.bpmn2.ocl;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.eclipse.bpmn2.BaseElement;
import org.eclipse.bpmn2.DocumentRoot;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.ocl.ParserException;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.util.EObjectUtil;
import carisma.modeltype.bpmn2.BPMN2Helper;
import carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot;
import carisma.modeltype.bpmn2.extended.MergeModels;
import carisma.modeltype.bpmn2.extension.Task;
import carisma.modeltype.bpmn2.extension.TaskSet;
import carisma.modeltype.bpmn2.extension.util.ExtensionUtil;
import carisma.modeltype.bpmn2.yaoqiang.YaoqiangHelper;
import carisma.ocl.OclEvaluator;
import carisma.ocl.OclQueryResult;
import carisma.ocl.library.OclExpression;
import carisma.ocl.library.OclLibrary;


/**
 * This Class performs an OCL query on a given bpmn2 Model.
 * @author Marcel Michel
 */
public class Check implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.bpmn2.ocl";

	public static final String PARAM_EXTENSIONFILE = "carisma.check.bpmn2.ocl.extensionfile";
	public static final String PARAM_OCL_SELECTION = "carisma.check.bpmn2.ocl.pattern";
	public static final String PARAM_MARK_VIOLATIONG_ELEMENTS = "carisma.check.bpmn2.ocl.markElements";
	public static final String PARAM_OCL_LIBRARY = "carisma.check.bpmn2.ocl.library";
	
	public static final String CHECK_NAME = "BPMN2 OCL Check";

	/**
	 * The AnalysisHost.
	 */
	private AnalysisHost host = null;

	/**
	 * The OclHelper.
	 */
	private OclHelper oclHelper = null;
	
	/**
	 * Regular expression which describes the extension pattern.
	 */
	private static final String REG_EXTFILE = "<<ext=\\w+\\.\\w+>>";
	
	/**
	 * Regular expression which describes the 'pattern' format.
	 */
	private static final String REG_PATTERN = "<<pattern=\\{(\\*|(\\w+(,\\w+)*))\\}>>";
	
	/**
	 * Represents success of the execution.
	 * Whenever an error occurred, successful will be set to false.
	 */
	private boolean successful = true;
	
	/**
	 * If true violating elements will be marked in the model.
	 */
	private boolean markElements = false;
	
	/**
	 * If true the model has been extended.
	 */
	private boolean isExtendedModel = false;
	
	/**
	 * If isExtendedModel true, the original resource
	 * reference will be stored here.
	 */
	private Resource originalResource = null;
	
	/**
	 * Line to divide the query results in the report.
	 */
	private static final String DIVIDING_LINE = "------------------------------------------------------------------------------------";

	/**
	 * Method tries to load file parameters, model and oclExpression. 
	 * If successful it delegates the query to the performQuery method.
	 * 
	 * @param parameters The PluginParameters
	 * @param analysisHost The AnalysisHost
	 * @return returns true if the query was successful otherwise false
	 */
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost analysisHost) {
		this.host = analysisHost;
		this.oclHelper = new OclHelper();

		Resource currentModel = analysisHost.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
					"No content loaded"));
			return false;
		}
		
		InputFileParameter oclFile = null;
		String pattern = null;
		String extensionFileName = null;
		if (parameters != null &&
				parameters.containsKey(Check.PARAM_OCL_LIBRARY) && 
				parameters.containsKey(Check.PARAM_MARK_VIOLATIONG_ELEMENTS) &&
				parameters.containsKey(Check.PARAM_OCL_SELECTION) &&
				parameters.containsKey(Check.PARAM_EXTENSIONFILE)) {
			
			oclFile = (InputFileParameter) 
					parameters.get(Check.PARAM_OCL_LIBRARY);
			this.markElements = ((BooleanParameter) 
					parameters.get(Check.PARAM_MARK_VIOLATIONG_ELEMENTS)).getValue();
			
			String tmp = "";
			tmp = ((StringParameter)
					parameters.get(Check.PARAM_OCL_SELECTION)).getValue();
			if (tmp != null && isShortPatternDefinition(tmp)) {
				pattern = tmp;
			}
			tmp = "";
			tmp = ((StringParameter)
					parameters.get(Check.PARAM_EXTENSIONFILE)).getValue();
			if (tmp != null && isShortExtDefinition(tmp)) {
				extensionFileName = tmp;
			}
		} else {
			analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
					"Could not resolve necessary parameters"));
			return false;
		}

		// The other parameters are represented as a text annotation in the bpmn2 model
		/*List<Documentation> docs;
		String pattern = null;
		String extensionFileName = null;
		if (currentModel.getContents().get(0) instanceof DocumentRoot) {
			docs = ((DocumentRoot) currentModel.getContents().get(0)).getDefinitions().getDocumentation();
			for (Documentation doc  : docs) {
				if (isExtDefinition(doc.getText())) {
					extensionFileName = getExtDefinition(doc.getText());
				} else if (isPatternDefinition(doc.getText())) {
					pattern = getPatternDefinition(doc.getText());
				}
			}
		}*/
		
		if (extensionFileName == null) {
			analysisHost.addResultMessage(new AnalysisResultMessage(
				StatusType.INFO, "No extension file defined"));
		} else {
			File extensionFile = new File(new File(currentModel.getURI().toFileString()).getParent() + "\\" + extensionFileName);
			if (extensionFile.exists()) {
				Resource extensionModel;
				try {
					extensionModel = ExtensionUtil.loadResource(extensionFile);
				} catch (IOException e) {
					extensionModel = null;
				}
				
				Resource tmp = MergeModels.run(currentModel, extensionModel);
				if (tmp != null) {
					this.originalResource = currentModel;
					currentModel = tmp;
					this.isExtendedModel = true;
				}
				analysisHost.addResultMessage(new AnalysisResultMessage(
						StatusType.INFO, "Extension file: " + extensionFileName));
			} else {
				analysisHost.addResultMessage(new AnalysisResultMessage(
						StatusType.ERROR, "Extension file '" + extensionFile.getAbsolutePath() + "' not found or could not be loaded!"));
				this.successful = false; 
			}
		}
		
		if (pattern != null) {
			analysisHost.addResultMessage(new AnalysisResultMessage(
					StatusType.INFO, "Patterns: " + pattern));
		} else {
			analysisHost.addResultMessage(new AnalysisResultMessage(
					StatusType.ERROR, "No query patterns defined. Must meet scheme: (*|(\\w+(,\\w+)*))"));
			return false;
		}
		
		if (oclFile.getValue() == null) {
			analysisHost.addResultMessage(new AnalysisResultMessage(
					StatusType.ERROR, "No OCL-File selected"));
			return false;
		}
		
		OclLibrary lib = null;
		List<OclExpression> ocl = null;
		try {
			lib = OclHelper.getOclLibrary(oclFile.getValue());
			analysisHost.addResultMessage(new AnalysisResultMessage(
					StatusType.INFO, "OCL-Library: " + lib.getName()));
			ocl = getOclExpression(lib.getOclExpressions(), pattern);
		} catch (Exception e) {
			analysisHost.addResultMessage(new AnalysisResultMessage(
					StatusType.ERROR, "Could not load OCL Expression: " + e.getMessage()));
			return false;
		}
		
		
		analysisHost.appendLineToReport(DIVIDING_LINE);
		return performQuery(currentModel, ocl);
	}

	/**
	 * Returns List of desired OCL-Expressions.
	 * 
	 * @param oclExpressions List of all OCL-Expressions
	 * @param selection The selection
	 * @return List of selected OCL-Expressions
	 */
	private List<OclExpression> getOclExpression(
			final EList<OclExpression> oclExpressions, final String selection) {
		
		ArrayList<OclExpression> result = new ArrayList<>();
		
		if (selection.equals("*") || selection.equals("all")) {
			for (OclExpression o : oclExpressions) {
				result.add(o);
			}
		} else {
			String[] arrSel = selection.split(",");
			
			Map<String, OclExpression> constraintMap = 
					new HashMap<>();
			for (OclExpression expression : oclExpressions) {
				constraintMap.put(expression.getName().toLowerCase(Locale.ENGLISH), expression);
			}
			
			for (String s : arrSel) {
				try {
					result.add(oclExpressions.get(Integer.parseInt(s)));
				} catch (NumberFormatException e) {
					
					if (constraintMap.containsKey(s.toLowerCase(Locale.ENGLISH))) {
						result.add(constraintMap.get(s.toLowerCase(Locale.ENGLISH)));
					} else {
						this.host.addResultMessage(new AnalysisResultMessage(
								StatusType.ERROR, "Could not resolve string '" + s + "'"));
						this.successful = false;
					}
				}
			}
		}
		return result;
	}

	/**
	 * Performs an OCL query on the currentModel with the oclExpression.<p>
	 * This method appends also an information report with all model elements 
	 * to the AnalysisHost which fits to the oclExpression.
	 * 
	 * @param currentModel The Model
	 * @param oclExpressions The OclExpressions
	 * @return returns true if the query was successful otherwise false
	 */
	private boolean performQuery(final Resource currentModel, final List<OclExpression> oclExpressions) {
		
		EObject content = currentModel.getContents().get(0);
		if (content instanceof DocumentRoot || content instanceof ExtendedDocumentRoot) {
			
			if (this.markElements) {
				if (this.isExtendedModel) {
					YaoqiangHelper.clearAllWarningAndInfoFlags(this.originalResource.getContents().get(0));
				} else {
					YaoqiangHelper.clearAllWarningAndInfoFlags(content);
				}
			}
			
			for (OclExpression oclExpression : oclExpressions) {
				
				String oclStatement = oclExpression.getQuery();
				
				EClass oclContext = null;
				if (!oclExpression.getContext().equalsIgnoreCase("context-free")) {
					oclContext = resolveOclContext(content, oclExpression.getContext());
					if (oclContext == null) {
						this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING,
								"Context '" + oclExpression.getContext() + "' not found. Querying model context-free"));
					}
				} else {
					this.host.addResultMessage(new AnalysisResultMessage(StatusType.INFO,
							"Querying model context-free"));
				}
				
				try {
					OclQueryResult qResult = OclEvaluator.query(content, oclContext, oclStatement);
					
					StatusType resultStatus;
					if (qResult.isViolated()) {
						resultStatus = StatusType.ERROR;
						this.successful = false;
					} else {
						resultStatus = StatusType.INFO;
					}
					
					this.host.addResultMessage(new AnalysisResultMessage(
							resultStatus, 
							oclExpression.getName() + (resultStatus == StatusType.INFO 
									?  " passed" : " violated (" 
										+  (qResult.size() > 1 
											? qResult.size() + " elements)" : "1 element)"))));
					generateReport(qResult);
					if (this.markElements) {						
						markViolatingElements(qResult);
					}
					
				} catch (ParserException e) {
					String err = oclExpression.getName() + ": Parser Exception during OCL query (" + e.getMessage() + ")";
					this.host.addResultMessage(new AnalysisResultMessage(
							StatusType.ERROR, err));
					this.host.appendLineToReport(err);
					this.host.appendLineToReport(DIVIDING_LINE);
					this.successful = false;
				}
			}
		}
		
		if (this.markElements) {
			try {
				if (this.isExtendedModel) {
					this.originalResource.save(Collections.EMPTY_MAP);
				} else
					currentModel.save(Collections.EMPTY_MAP);
			} catch (IOException e) {
				this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
						"Error during saving: " + e.getMessage()));
			}
		}
		
		return this.successful;
	}
	
	/**
	 * Resolves the context class of a given context string.
	 * @param documentRoot The root of a standard or an extended bpmn2 model
	 * @param contextString The given context string
	 * @return If successful it will return the context class as an EClass Object otherwise null 
	 */
	private EClass resolveOclContext(EObject documentRoot, String contextString) {
		if (documentRoot instanceof DocumentRoot) {
			return this.oclHelper.getContextClass(contextString);
		} else if (documentRoot instanceof ExtendedDocumentRoot) {
			return this.oclHelper.getExtendedContextClass(contextString);
		} else {
			return null;
		}
	}
	
	/**
	 * Marks elements in the bpmn2 model, which violates the query result.
	 * @param query The OCL query result
	 */
	private void markViolatingElements(final OclQueryResult query) {
		if (query.isViolated()) {
			for (EObject elem : query.getElements()) {
				if (elem instanceof BaseElement) {
					setWarningFlagToElement((BaseElement) elem, query.getContextString(), query.getStatement());
					
					if (this.isExtendedModel) {
						EObject originalModel = this.originalResource.getContents().get(0);
						BaseElement originalElem = BPMN2Helper.findBaseElementById(originalModel, ((BaseElement) elem).getId());
						if (originalElem != null) {
							setWarningFlagToElement(originalElem, query.getContextString(), query.getStatement());
						} else {
							this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, 
									"Element '" + EObjectUtil.getName(elem) + "' could not be found in the model."));
						}
					}
				} else if (elem instanceof TaskSet) {
					List<Task> taskList = ((TaskSet) elem).getSelectedTasks();
					for (Task task : taskList) {
						EObject originalModel = this.originalResource.getContents().get(0);						
						BaseElement originalElem = BPMN2Helper.findBaseElementById(originalModel, task.getId());
						if (originalElem != null) {
							setWarningFlagToElement(originalElem, query.getContextString(), query.getStatement());
						} else {
							this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, 
									"Element '" + EObjectUtil.getName(elem) + "' could not be found in the model."));
						}
					}
				}
				else {
					this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, 
							"Element '" + EObjectUtil.getName(elem) + "' could not be marked. It is not a Bpmn2 model element."));
				}
			}
		}
	}
	
	/**
	 * Sets a Warning flag to an bpmn2element.
	 * @param baseElement The element which should be marked
	 * @param context The violated context string
	 * @param statement The violated statement string 
	 */
	private static void setWarningFlagToElement(BaseElement baseElement, String context, String statement) {
		YaoqiangHelper.setWarningFlagToBpmn2Element(baseElement, "Vioalates Constraint: [ " +
				context + " | " + statement + " ]");
	}
	
	/**
	 * Generates a report.
	 * 
	 * @param query The OCL query result stored in an object
	 */
	private void generateReport(final OclQueryResult query) {
		if (query.isViolated()) {
			this.host.appendLineToReport("Violated: [ " + query.getContextString() + " | " + query.getStatement() + " ]");
			for (EObject elem : query.getElements()) {
				this.host.appendLineToReport("\t" + elem.eClass().getName()  
						+ " | " + EObjectUtil.getName(elem));
			}
		} else {
			this.host.appendLineToReport("Passed: [ " + query.getContextString() + " | " + query.getStatement() + " ]");
		}
		this.host.appendLineToReport(DIVIDING_LINE);
	}
	
	/**
	 * Checks if the pattern REG_EXTFILE: <<ext=\w+\.\w+>> is applied correctly.
	 * 
	 * Samples: <<ext=test.bpmn>> 	valid
	 * 			<<ext=test>> 		invalid
	 * 			<<ext=.bpmn>> 		invalid
	 * 
	 * @param str The string which should be checked
	 * @return If string matches the pattern return true otherwise false
	 */
	@SuppressWarnings("unused")
	private static boolean isExtDefinition(final String str) {
		return str.matches(REG_EXTFILE);
	}
	
	/**
	 * Checks if the pattern REG_EXTFILE: \w+\.\w+ is applied correctly.
	 * 
	 * @param str The string which should be checked
	 * @return If string matches the pattern return true otherwise false
	 */
	private static boolean isShortExtDefinition(final String str) {
		return str.matches(REG_EXTFILE.substring(6, REG_EXTFILE.length() - 2));
	}
	
	/**
	 * Checks if the pattern REG_PATTERN: <<pattern=\{(*|(\w+(,\w+)*))\}>> is applied correctly.
	 * 
	 * Samples: <<pattern={pattern1,pattern2}>> valid
	 * 			<<pattern={pattern1}>> 			valid
	 * 			<<pattern=pattern>> 			invalid
	 * 			<<pattern={pattern,}>> 			invalid
	 * 
	 * @param str The string which should be checked
	 * @return If string matches the pattern return true otherwise false
	 */
	@SuppressWarnings("unused")
	private static boolean isPatternDefinition(final String str) {
		return str.matches(REG_PATTERN);
	}
	
	/**
	 * Checks if the pattern REG_PATTERN: (*|(\w+(,\w+)*)) is applied correctly.
	 * 
	 * @param str The string which should be checked
	 * @return If string matches the pattern return true otherwise false
	 */
	private static boolean isShortPatternDefinition(final String str) {
		return str.matches(REG_PATTERN.substring(13, REG_PATTERN.length() - 5));
	}
	
	/**
	 * Method will cut the extension definition string and will only
	 * return the extension file name.
	 * 
	 * Sample:
	 * 			Input:	<<ext=extension.bpmn2extension>>
	 * 			Output: extension.bpmn2extension
	 * 
	 * @param str String which fits to the extension definition
	 * @return Returns inner string
	 */
	@SuppressWarnings("unused")
	private static String getExtDefinition(final String str) {
		return str.substring(6, str.length() - 2);
	}
	
	/**
	 * Method will cut the pattern definition string and will only
	 * return a set of patterns, which are separated by comma.
	 * 
	 * Sample:
	 * 			Input:  <<pattern={pattern1,pattern2}>>
	 * 			Output: pattern1,pattern2
	 * @param str String which fits to the pattern definition
	 * @return Returns inner string, consisting of a set of patterns
	 */
	@SuppressWarnings("unused")
	private static String getPatternDefinition(final String str) {
		return str.substring(11, str.length() - 3);
	}

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}
	
}
