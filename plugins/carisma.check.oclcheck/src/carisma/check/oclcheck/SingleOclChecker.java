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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.StringParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;
import carisma.core.checks.ParameterType;
import carisma.ocl.OclEvaluator;


/**
 * Performs queries on models with a single OCL expression. 
 *  
 * @author Sebastian Haronski
 */
public class SingleOclChecker extends AbstractOclChecker {

	public static final String CHECK_ID = "carisma.check.singleoclcheck";
	public static final String PARAM_CONTEXT = "carisma.check.oclchecker.context";
	public static final String PARAM_STATEMENT = "carisma.check.oclchecker.statement";
	public static final String CHECK_NAME = "SingleOclChecker";

	/**
	 * The ocl-expression for the query.
	 */
	private String statement = null;
	
	/**
	 * The context class for the query.
	 */
	private EClass context = null;
	
	/**
	 * The host for the query.
	 */
	private AnalysisHost analysisHost = null;
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
				
		this.analysisHost = host;
		
		//check parameters
		List<CheckParameter> desiredParameters = new ArrayList<>();
		desiredParameters.add(new StringParameter(
				new CheckParameterDescriptor(PARAM_CONTEXT, "", "", ParameterType.STRING, true, "")));
		desiredParameters.add(new StringParameter(
				new CheckParameterDescriptor(PARAM_STATEMENT, "", "", ParameterType.STRING, true, "")));
		desiredParameters = resolveParameters(parameters, desiredParameters);
		
		if (desiredParameters == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
					"Could not resolve necessary parameters"));
			return false;
		} 
		
		String contextString = ((StringParameter) desiredParameters.get(0)).getValue().trim();
		this.statement = ((StringParameter) desiredParameters.get(1)).getValue().trim();

		
		if (contextString == null || this.statement == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
					"Parameters not set"));
			return false;
		}

		//parse string to class
		this.context = null;
		if (!contextString.equalsIgnoreCase(OclEvaluator.CONTEXT_FREE)) {
		
			this.context = findContextInPackage(contextString);
			
			if (this.context == null) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING,
						"Model contains no " + contextString + " object."));
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO,
						"Constraint passed: Context = '" + contextString + "' Statement = '" + this.statement + "'"));
				host.appendLineToReport("Constraint passed: Context = '" + contextString + "' Statement = '" + this.statement
										+ "' - No object of type '" + contextString + "' in model.");
				return true;	
			}
		}
		
		return super.performOclQuery(host);
	}
	
	
	@Override
	protected final String getOclStatement() {
		return this.statement;
	}

	@Override
	protected final EClass getOclContext() {
		return this.context;
	}

	/**
	 * Searches a class in the packages of a given host.
	 * @param contextString The name of the searched class.
	 * @return The class represented by the parameter contextString, if 
	 * no class could be found null is returned.
	 */
	private EClass findContextInPackage(final String contextString) {
		
		if (this.analysisHost == null) {
			return null;
		}
		
		EObject tmpObject = null;
		
		//list packages of model
		TreeIterator<EObject> modelIterator = this.analysisHost.getAnalyzedModel().getAllContents();
		Set<EPackage> pacSet = new HashSet<>();
		
		while (modelIterator.hasNext()) {
			tmpObject = modelIterator.next();
			pacSet.add(tmpObject.eClass().getEPackage());
		}
		
		//list sub- / superpackages
		Set<EPackage> packagesToAdd = new HashSet<>();
		
		for (EPackage epac : pacSet) {
			packagesToAdd.addAll(epac.getESubpackages());
			
			EPackage superPac = epac.getESuperPackage();
			while (superPac != null && packagesToAdd.add(superPac)) {
				superPac = superPac.getESuperPackage(); 
			}
		}
		pacSet.addAll(packagesToAdd);
	
		//search in packages
		for (EPackage epac : pacSet) {
			TreeIterator<EObject> content = epac.eAllContents();
			while (content.hasNext()) {
				tmpObject = content.next();
				if (tmpObject instanceof EClass 
						&& ((EClass) tmpObject).getName().equalsIgnoreCase(contextString)) {
					return (EClass) tmpObject;
				}
			}
		}
		
		return null;
		
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
