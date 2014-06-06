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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.junit.Test;

import carisma.check.oclcheck.SingleOclChecker;
import carisma.check.oclcheck.util.DebugHost;
import carisma.check.oclcheck.util.UniversalModelLoader;
import carisma.core.models.ModelLoader;


/**
 * Test class.
 * @author Marcel Michel
 * 
 */
public class SingleOclCheckerTest {
	
	/**
	 * The path to the model directory. 
	 */
	private String filepath = "resources/models/";
	
	/**
	 * Method to load a model.
	 * @param testmodelname The model name
	 * @param modelLoader The model loader
	 * @return If successful the model as resource otherwise null
	 * 
	 */
	public final Resource loadModel(final ModelLoader modelLoader, final String testmodelname) {
		File testmodelfile = new File(filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		try {
			return modelLoader.load(testmodelfile);
		} catch (IOException e) {
			fail(e.getMessage());
			return null;
		}
	}
	
	/**
	 * Checks if each class of a given model is resolved correctly by name.
	 */
	@Test
	public final void findContextInPackageTest() {
		String methodName = "findContextInPackage";
		UniversalModelLoader modelLoader = new UniversalModelLoader();
		DebugHost host = new DebugHost();
		SingleOclChecker oclChecker = new SingleOclChecker();
		
		for (String modelname : new String[] {
				"bpmn2model.bpmn2", 
				"activitydiagram.uml",
				"classdiagram.uml",
				"componentdiagram.uml",
				"compositediagram.uml",
				"deploymentdiagram.uml",
				"sequencediagram.uml",
				"statemachinediagram.uml",
				"usecasediagram.uml"}) {
			Resource model = loadModel(modelLoader, modelname);
			host.setAnalyzedModel(model);
			assertNotNull(model);
			
			
			final Method[] methods = oclChecker.getClass().getDeclaredMethods();
			int methodIndex = -1;
			
			for (int i = 0; i < methods.length; i++) {
				if (methods[i].getName().equals(methodName)) {
					methodIndex = i;
					methods[i].setAccessible(true);
					try {
						
						Field analysisHostField = oclChecker.getClass().getDeclaredField("host");
						analysisHostField.setAccessible(true);
						analysisHostField.set(oclChecker, host);
						
						TreeIterator<EObject> iterator = model.getAllContents();
						while (iterator.hasNext()) {
							EObject obj = iterator.next();
							assertEquals(obj.eClass().getName(), 
									((EClass) methods[i].invoke(oclChecker, obj.eClass().getName())).getName());
						}
						

					} catch (Exception e) {
						fail("Error during invoke (" + modelname + ")");
					}
					break;
				}
			}
			assertNotSame(-1, methodIndex);
		}
	}
		
}
