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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckParameterDescriptor;
import carisma.core.checks.ParameterType;
import carisma.ocl.OclEvaluator;
import carisma.ocl.library.OclExpression;
import carisma.ocl.library.OclLibrary;



/**
 * Performs queries on BPMN and UML models with a set of OCL expressions. 
 *  
 * @author Sebastian Haronski
 */ 
public class MultiOclChecker extends AbstractOclChecker {
	
	public static final String CHECK_ID = "carisma.check.multioclcheck";
	public static final String PARAM_QUERY_FILE = "carisma.check.oclcheck.oclfile";
	public static final String CHECK_NAME = "MultiOclChecker";

	/**
	 * The ocl-expression for the query.
	 */
	private String statement = null;
	
	/**
	 * The context class for the query.
	 */
	private EClass context = null;
	
	/**
	 * A map containing all UML context classes as both strings and instances.
	 * Used for mapping context expressions to instances.
	 */
	private Map<String, EClass> contextMap = null;
	
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
		
		//check parameter
		List<CheckParameter> desiredParameters = new ArrayList<>();
		desiredParameters.add(new InputFileParameter(
				new CheckParameterDescriptor(PARAM_QUERY_FILE, "", "", ParameterType.INPUTFILE, true, "")));
		desiredParameters = resolveParameters(parameters, desiredParameters);
		
		if (desiredParameters == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR,
					"Could not resolve necessary parameters"));
			return false;
		} 
		
		File oclFile = ((InputFileParameter) desiredParameters.get(0)).getValue();
		
		
		//load ocl expressions from file
		OclLibrary lib = null;
		List<OclExpression> oclExpressions = null;
		
		try {
			lib = getOclLibrary(oclFile);
			if (lib != null) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "OCL-Library: " + lib.getName()));
			}
		} catch (Exception e) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, 
					"Could not load OCL library: " + e.getMessage()));
			return false;
		}
		
		if (lib != null) {
			oclExpressions = lib.getOclExpressions();
			if (oclExpressions.size() == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING,	"Library contains no constraints."));
			}
		}
		

		//check model and instantiate hashmap
		Resource model = host.getAnalyzedModel();
		
		if (model == null || model.getContents() == null) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Model is empty."));
			return false;	
		}
		
		createMapWithPackages(model);
		
		//check models with ocl queries
		int missed = 0;
		if (oclExpressions != null) {
			for (OclExpression expr : oclExpressions) {
				
				this.statement = expr.getQuery().trim();
				this.context = this.contextMap.get(expr.getContext().toLowerCase(Locale.ENGLISH).trim());
				
				if (this.context == null && !expr.getContext().equalsIgnoreCase(OclEvaluator.CONTEXT_FREE)) {
					host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING,
							"Model contains no " + expr.getContext() + " object."));
					host.addResultMessage(new AnalysisResultMessage(StatusType.INFO,
							"Constraint passed: Context = '" + expr.getContext() + "' Statement = '" + this.statement + "'"));
					host.appendLineToReport("Constraint passed: Context = '" + expr.getContext() + "' Statement = '" + this.statement
											+ "' - No object of type '" + expr.getContext() + "' in model.");
				} else {
					if (!super.performOclQuery(host)) {
						missed++;
					}
				}
				
				host.appendLineToReport("");
			}
		}
		
		
		//check number of missed constraints
		if (missed > 0) {
			if (oclExpressions == null) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "OCL expressions undefined."));
				host.appendLineToReport("OCL expressions undefined.");
			} else {
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING,
						missed + " out of " + oclExpressions.size() + " constraints violated."));
				host.appendLineToReport(missed + " out of " + oclExpressions.size() + " constraints violated.");
			}
			
			return false;
		}
				
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "All constraints passed."));
		return true; 
	}
	
	
	/**
	 * Loads an OCL-Library.
	 * 
	 * @param file The OCL-Library File
	 * @return If successful the method returns an OCL-Library otherwise null 
	 * @throws IOException If the OCL-Library could not be loaded
	 */
	private static OclLibrary getOclLibrary(final File file) throws IOException {
		URI uri = URI.createFileURI(file.getAbsolutePath());
		ResourceSet resourceSet = new ResourceSetImpl();
		Resource resource = resourceSet.getResource(uri, true);
		resource.load(new HashMap<String, Object>());
		
		EObject content = resource.getContents().get(0);
		if (content instanceof OclLibrary) { 
			return (OclLibrary) resource.getContents().get(0);
		}
		return null;
	}
	
	/**
	 * Creates a hashmap containing the name and EClass of classes in the packages of the model.
	 * @param model The model used for creating the hashmap.
	 */
	private void createMapWithPackages(final Resource model) {
			
		if (model == null) {
			this.contextMap = null;
			return;
		}
				
		Map<String, EClass> contextMapTemp = new HashMap<>();
		
		//list packages of model
		EObject tmpObject = null;
		TreeIterator<EObject> modelIterator = model.getAllContents();
		Set<EPackage> pacSet = new HashSet<>();
		
		while (modelIterator.hasNext()) {
			tmpObject = modelIterator.next();
			pacSet.add(tmpObject.eClass().getEPackage());
		}
		
		//list sub- / superpackages
		for (EPackage epac : pacSet) {
			pacSet.addAll(epac.getESubpackages());
			
			EPackage superPac = epac.getESuperPackage();
			while (superPac != null && !pacSet.contains(superPac)) {
				pacSet.add(superPac);
			}
		}
		
		//search in packages
		for (EPackage epac : pacSet) {
			TreeIterator<EObject> content = epac.eAllContents();
			while (content.hasNext()) {
				tmpObject = content.next();
				
				if (tmpObject instanceof EClass && !contextMapTemp.containsValue(tmpObject)) {
					contextMapTemp.put(((EClass) tmpObject).getName().toLowerCase(Locale.ENGLISH), (EClass) tmpObject);				
				}	
			}
		}
			
		this.contextMap = Collections.unmodifiableMap(contextMapTemp);
	}
	

	@Override
	protected final String getOclStatement() {
		return this.statement;
	}

	@Override
	protected final EClass getOclContext() {
		return this.context;
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
