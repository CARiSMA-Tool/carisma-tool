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
package carisma.core;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.Analyzer;
import carisma.core.analysis.UIConnector;
import carisma.core.analysis.result.AnalysisResult;
import carisma.core.checks.CheckRegistry;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.models.ModelManager;
import carisma.core.models.ModelTypeRegistry;
import carisma.core.preferences.PreferenceManager;


/**
 * The activator class controls the plug-in life cycle.
 */
public class Carisma implements BundleActivator {

	/**
	 *  The plug-in ID.
	 */
	public static final String PLUGIN_ID = "carisma.core"; //$NON-NLS-1$
	
	/**
	 * IDs of defined extension points
	 */
	public static final String EXTENSION_POINT_CARISMACHECK = "carisma.carismacheck"; //$NON-NLS-1$
	public static final String EXTENSION_POINT_CARISMAMODELTYPE = "carisma.modeltype"; //$NON-NLS-1$
	
	/**
	 *  The shared instance.
	 */
	private static Carisma instance;
	
	
	/** 
	 * Getter for the shared instance.
	 * @return the actual instance.
	 */

	public static Carisma getInstance() {
		if (instance==null) {
			instance = new Carisma();
		}
		return instance;
	}

	/**
	 * 
	 */
	private CheckRegistry checkRegistry;
	/**
	 * 
	 */
	private ModelTypeRegistry modelTypeRegistry;
	/**
	 * 
	 */
	private ModelManager modelManager;
	/**
	 * 
	 */
	private Analyzer analyzer;
	/**
	 * 
	 */
	private List<AnalysisResult> analysisResults;
	
	private PreferenceManager preferenceManager;

	public static final String EXTENSION_POINT_CARISMA_CARISMACHECK = "carisma.carismacheck";

	/**
	 * The Carisma constructor.
	 */
	public Carisma() {
		instance = this;
		this.checkRegistry = new CheckRegistry();
		this.modelTypeRegistry = new ModelTypeRegistry();
		this.modelManager = new ModelManager();
		this.analysisResults = new ArrayList<>();
		this.analyzer = new Analyzer();
		this.preferenceManager = new PreferenceManager();
		loadPreferences();
	}
	
	/**
	 * 
	 * @return List<AnalysisResult>
	 */
	public final List<AnalysisResult> getAnalysisResults() {
		return this.analysisResults;
	}
	
	protected void loadPreferences() {
		String prefPath = System.getProperty("user.home")+System.getProperty("file.separator")+"carisma.cfg";
		File prefFile = new File(prefPath);
		if (prefFile.exists()) {
			this.preferenceManager.loadPreferences(prefPath);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.core.runtime.Plugin#start(org.osgi.framework.BundleContext
	 * )
	 */
	/**
	 * @param context
	 * @throws Exception an Exception
	 */
	@Override
	public final void start(final BundleContext context) throws Exception {
		//super.start(context);
		this.modelTypeRegistry.initialize();
		this.checkRegistry.initialize();
		activateProfiles(context);
	}
	
	/**
	 * 
	 * @param context BundleContext
	 */
	private static void activateProfiles(final BundleContext context) {
		for (Bundle b : context.getBundles()) {
			if (b.getSymbolicName() != null 
				&& b.getSymbolicName().startsWith("carisma.profile") 
				&& !b.getSymbolicName().endsWith(".tests") 
				&& b.getState() != Bundle.ACTIVE 
				&& b.getState() != Bundle.STARTING) {
				try {
					b.start();
				} catch (BundleException e) {
					Logger.log(LogLevel.ERROR, "Bundle, " + b.getBundleId() + ", could not be started.", e);
				}
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.core.runtime.Plugin#stop(org.osgi.framework.BundleContext
	 * )
	 */
	/**
	 * @param context BundleContext
	 * @throws Exception  
	 */
	@Override
	public final void stop(final BundleContext context) throws Exception {
		instance = null;
		//super.stop(context);
	}
	
	/**
	 * 
	 * @return this CheckRegistry
	 */
	public final CheckRegistry getCheckRegistry() {
		return this.checkRegistry;
	}

	/**
	 * 
	 * @return this model type registry
	 */
	public final ModelTypeRegistry getModelTypeRegistry() {
		return this.modelTypeRegistry;
	}

	/**
	 * 
	 * @return modelManager
	 */
	public final ModelManager getModelManager() {
		return this.modelManager;
	}

	/**
	 * 
	 * @return modelManager
	 */
	public final PreferenceManager getPreferenceManager() {
		return this.preferenceManager;
	}

	/**
	 * 
	 * @param analysis the analysis to be run
	 */
	public final void runAnalysis(final Analysis analysis, final UIConnector connector) {
		this.analyzer.runAnalysis(analysis, connector);
	}

	/**
	 * 
	 */
	public final void reset() {
		this.analysisResults.clear();
	}

}
