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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import org.eclipse.emf.ecore.resource.Resource;
import org.junit.Test;

import carisma.check.oclcheck.util.DebugHost;
import carisma.check.oclcheck.util.DummyOclChecker;
import carisma.check.oclcheck.util.UniversalModelLoader;
import carisma.core.models.ModelLoader;


/**
 * Test class.
 * @author Marcel Michel
 * 
 */
public class AbstrOclCheckerTest {
	
	/**
	 * The path to the model directory. 
	 */
	private String filepath = "resources/models/";

	/**
	 * Simulates a checker instance.
	 */
	private DummyOclChecker oclChecker;
	
	/**
	 * Simulates an analysis host.
	 */
	private DebugHost host;
	
	/**
	 * Universal resource loader.
	 */
	private UniversalModelLoader modelLoader;
	
	/**
	 * Method to load a model.
	 * @param testmodelname The model name
	 * @param modelLoader The model loader
	 * @return If successful the model as resource otherwise null
	 * 
	 */
	public final Resource loadModel(final ModelLoader modelLoader, final String testmodelname) {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		try {
			return modelLoader.load(testmodelfile);
		} catch (IOException e) {
			fail(e.getMessage());
			return null;
		}
	}
	
	/**
	 * Test queries a bpmn2 model.
	 */
	@Test
	public final void oclQueryBpmn2ModelTest() {
		this.oclChecker = new DummyOclChecker();
		this.host = new DebugHost();
		this.modelLoader = new UniversalModelLoader();
		
		boolean successful;

		Resource model;
		model = loadModel(this.modelLoader, "bpmn2model.bpmn2");
		assertNotNull(model);
		this.host.setAnalyzedModel(model);
		
		this.oclChecker.setOclStatement("true");
		successful = this.oclChecker.performOclQuery(this.host);
		assertTrue(successful);
		
		this.oclChecker.setOclStatement("false");
		successful = this.oclChecker.performOclQuery(this.host);
		assertFalse(successful);
	}
	
	
	/**
	 * Test queries a uml2 model.
	 */
	@Test
	public final void oclQueryUml2ModelTest() {
		this.oclChecker = new DummyOclChecker();
		this.host = new DebugHost();
		this.modelLoader = new UniversalModelLoader();
		
		boolean successful;

		Resource model;
		model = loadModel(this.modelLoader, "activitydiagram.uml");
		assertNotNull(model);
		this.host.setAnalyzedModel(model);
		
		this.oclChecker.setOclStatement("true");
		successful = this.oclChecker.performOclQuery(this.host);
		assertTrue(successful);
		
		this.oclChecker.setOclStatement("false");
		successful = this.oclChecker.performOclQuery(this.host);
		assertFalse(successful);
	}
}
