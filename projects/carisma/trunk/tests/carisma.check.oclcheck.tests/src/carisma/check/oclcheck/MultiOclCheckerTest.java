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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Locale;
import java.util.Map;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.junit.Test;

import carisma.check.oclcheck.MultiOclChecker;
import carisma.check.oclcheck.util.UniversalModelLoader;
import carisma.core.models.ModelLoader;
import carisma.ocl.library.OclLibrary;


/**
 * Test class.
 * @author Marcel Michel
 * 
 */
public class MultiOclCheckerTest {

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
	 * Test will load a dummy OCL Library and will compare the entries.
	 */
	@Test 
	public final void loadOclLibraryTest() {
		String testmodelfilename = "testLibrary.col";
		File testmodelfile = new File(this.filepath + File.separator + testmodelfilename);
		assertTrue(testmodelfile.exists());
		
		OclLibrary oclLibrary = null;
		
		MultiOclChecker oclChecker = new MultiOclChecker();
		String methodName = "getOclLibrary";
		Method[] methods  = oclChecker.getClass().getDeclaredMethods();
		int methodIndex = -1;
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methods[i].setAccessible(true);
				methodIndex = i;

				try {
					oclLibrary = (OclLibrary) methods[i].invoke(oclChecker, testmodelfile);
					
					assertNotNull(oclLibrary);
					assertTrue(oclLibrary.getName().equals("Library"));
					assertTrue(oclLibrary.getOclExpressions().get(0).getContext().equals("context"));
					assertTrue(oclLibrary.getOclExpressions().get(0).getDescription().equals("description"));
					assertTrue(oclLibrary.getOclExpressions().get(0).getName().equals("name"));
					assertTrue(oclLibrary.getOclExpressions().get(0).getQuery().equals("query"));
				} catch (Exception e) {
					fail("Error during invoke");
				}
				break;
			}
		}
		
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
	
	/**
	 * Test will verify that all eClasses of a model are included in the generated contextMap.
	 */
	@SuppressWarnings("unchecked")
	@Test
	public final void createMapWithPackagesTest() {
		String methodName = "createMapWithPackages";
		UniversalModelLoader modelLoader = new UniversalModelLoader();
		MultiOclChecker oclChecker = new MultiOclChecker();
		
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
			assertNotNull(model);
			
			
			final Method[] methods = oclChecker.getClass().getDeclaredMethods();
			int methodIndex = -1;
			
			for (int i = 0; i < methods.length; i++) {
				if (methods[i].getName().equals(methodName)) {
					methodIndex = i;
					methods[i].setAccessible(true);
					try {
						
						methods[i].invoke(oclChecker, model);
						
						Map<String, EClass> contextMap = null;
						Field fContextMap = oclChecker.getClass().getDeclaredField("contextMap");
						fContextMap.setAccessible(true);
						contextMap = (Map<String, EClass>) fContextMap.get(oclChecker);
						
						assertNotNull(contextMap);
						
						TreeIterator<EObject> iterator = model.getAllContents();
						while (iterator.hasNext()) {
							EObject eObj = iterator.next();
							EClass eClass = eObj.eClass(); 
							assertTrue(contextMap.containsKey(eClass.getName().toLowerCase(Locale.ENGLISH)));
						}
					} catch (Exception e) {
						fail("Error during invoke (" + modelname + ")");
					}
					break;
				}
			}
			assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
		}
	}
}
