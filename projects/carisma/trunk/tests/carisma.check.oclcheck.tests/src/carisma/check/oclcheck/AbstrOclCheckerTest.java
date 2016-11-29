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
import java.io.File;
import java.io.IOException;
import java.util.Collections;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.junit.Test;

import carisma.check.oclcheck.util.DebugHost;
import carisma.check.oclcheck.util.DummyOclChecker;


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
	private ResourceSet rs = new ResourceSetImpl();
	
	/**
	 * Method to load a model.
	 * @param testmodelname The model name
	 * @param modelLoader The model loader
	 * @return 
	 * @return If successful the model as resource otherwise null
	 * 
	 */
	 private Resource loadModel(final String testmodelname) throws IOException {
	     File testmodelfile = new File(this.filepath + File.separator + testmodelname);
	     assertTrue(testmodelfile.exists());
	     Resource modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));      
	     modelres.load(Collections.EMPTY_MAP);
	     return modelres;
	}
	
	 /**
	 * Test queries a bpmn2 model.
	 * @throws IOException 
	 */
	@Test
	public final void oclQueryBpmn2ModelTest() throws IOException {
		this.oclChecker = new DummyOclChecker();
		this.host = new DebugHost();
		
		boolean successful;

		Resource model;
		model = loadModel("bpmn2model.bpmn2");
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
	 * @throws IOException 
	 */
	@Test
	public final void oclQueryUml2ModelTest() throws IOException {
		this.oclChecker = new DummyOclChecker();
		this.host = new DebugHost();
		
		boolean successful;

		Resource model;
		model = loadModel("activitydiagram.uml");
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
