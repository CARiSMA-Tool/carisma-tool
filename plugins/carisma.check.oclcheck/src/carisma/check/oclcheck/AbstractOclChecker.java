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
package carisma.check.oclcheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.ocl.ParserException;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.util.EObjectUtil;
import carisma.ocl.OclEvaluator;
import carisma.ocl.OclQueryResult;


/**
 * This Class performs an OCL-Query on a given EMF-Model.
 * @author Marcel Michel
 *
 */
public abstract class AbstractOclChecker implements CarismaCheckWithID {

	/**
	 * The AnalysisHost.
	 */
	private AnalysisHost analysisHost = null;
	
	/**
	 * String for output.
	 */
	private static final String STATEMENT = "' Statement = '";
	
	/**
	 * Abstract method to implement the CarismaCheckWithID Interface.
	 * 
	 * @param parameters The PluginParameters
	 * @param host The AnalysisHost
	 * @return returns true if the query was successful otherwise false
	 */
	@Override
	public abstract boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host);
	
	/**
	 * Method tries to load model and uses the returns of getOclContext() and getOclStatement to
	 * perform an ocl-query on the given model using the query method. If getOclContext() returns
	 * null, a context free query is performed. 
	 * 
	 * @param host The AnalysisHost
	 * @return returns true if the query was successful otherwise false
	 */
	public final boolean performOclQuery(final AnalysisHost host) {
		this.analysisHost = host;
		
		EClass contextClass = getOclContext();
		String statement = getOclStatement();
		boolean violated = false;
		
		if (statement == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
					"Parameters not set"));
			return false;
		}
		
		String context = "";
		if (contextClass == null) {
			context = OclEvaluator.CONTEXT_FREE;
		} else {
			context = contextClass.getName();
		}

		OclQueryResult tmpResult = null;
		ArrayList<OclQueryResult> result = new ArrayList<>();
		try {
			for (EObject content : host.getAnalyzedModel().getContents()) {
				tmpResult = OclEvaluator.query(content, contextClass, statement);
				if (tmpResult != null && tmpResult.isViolated()) {
					violated = true;
				}
				result.add(tmpResult);
			}
		} catch (ParserException e) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
					"Error during OCL-Query: "  + e.getMessage()));
			host.appendLineToReport("Error during query: Context = '" + context + STATEMENT + statement + "'");
			host.appendLineToReport(e.getMessage());
			violated = true;
		}
		
		generateReport(result);
		
		if (violated) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING,
					"Constraint violated: Context = '" + context + STATEMENT + statement + "' - See report for more information."));
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO,
					"Constraint passed: Context = '" + context + STATEMENT + statement + "'"));
		}
		
		return !violated;
	}
	
	/**
	 * Generates a report.
	 * 
	 * @param resultList List of elements which does not fulfill the OCL-Constraint
	 */
	private void generateReport(final List<OclQueryResult> resultList) {
		for (OclQueryResult query : resultList) {
			if (query.isViolated()) {
				this.analysisHost.appendLineToReport("Constraint violated: Context = '" + query.getContextString() + STATEMENT + query.getStatement() + "'");
				this.analysisHost.appendLineToReport("List of violating objects:");
				for (EObject elem : query.getElements()) {
					this.analysisHost.appendLineToReport("\t Class = '" + elem.eClass().getName()  
							+ "' Name = '" + EObjectUtil.getName(elem) + "'");	
				}
			} else {
				this.analysisHost.appendLineToReport("Constraint passed: Context = '" + query.getContextString() + STATEMENT + query.getStatement() + "'");
			}
		}
	}
	
	
	/**
	 * Returns the OCL-Statement.
	 * 
	 * @return returns the modified OCL-Statement
	 */
	protected abstract String getOclStatement();
	
	/**
	 * Returns the OCL-Context Class.
	 * 
	 * @return returns the OCL-Context as EClass
	 */
	protected abstract EClass getOclContext();
	
	/**
	 * Collects CheckParameters from  List 'parameters', which are
	 * specified in the List 'desiredParameters'.
	 * The order of the collected CheckParameters is identical to the
	 * desiredParamaters List.
	 * 
	 * @param parameters Pool of parameters
	 * @param desiredParameters Desired parameters
	 * @return If all desired parameters are in the pool, the resolved list
	 * will be returned, otherwise null
	 */
	protected final static List<CheckParameter> resolveParameters(final Map<String, CheckParameter> parameters, 
			final List<CheckParameter> desiredParameters) {
		List<CheckParameter> result = new ArrayList<>();
		for (int i = 0; i < desiredParameters.size(); i++) {
			CheckParameter foundParameter = parameters.get(desiredParameters.get(i).getDescriptor().getID());
			if (foundParameter != null) {
				result.add(foundParameter);
			}
//			for (int j = 0; j < parameters.size(); j++) {
//				if (desiredParameters.get(i).getDescriptor().equals(
//						parameters.get(j).getDescriptor())) {
//					result.add(parameters.get(j));
//					found = true;
//					break;
//				}
//			}
//			if (!found) {
//				break;
//			}
		}
		
		return result.size() == desiredParameters.size() ? result : null;
	}
}