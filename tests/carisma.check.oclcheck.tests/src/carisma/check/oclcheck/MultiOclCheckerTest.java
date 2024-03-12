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
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;


import java.util.logging.Logger;


/**
 * Test class.
 * @author Marcel Michel
 * 
 */
@RunWith(Parameterized.class)
public class MultiOclCheckerTest {

	private static final Logger logger = Logger.getLogger(MultiOclCheckerTest.class.getName());
	
	/**
	 * The path to the model directory. 
	 */
	private String filepath = "resources/models/";
	
	/**
	 * Test parameters
	 */
	private String modelname, methodname;
	
	public MultiOclCheckerTest(String modelname, String methodname) {
		this.modelname = modelname;
		this.methodname = methodname;
	}
	
	/**
	 * Method to load a model.
	 * @param testmodelname The model name
	 * @param modelLoader The model loader
	 * @return If successful the model as resource otherwise null
	 * 
	 */
	public final Resource loadModel(final String testmodelname) {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		try (FileInputStream in = new FileInputStream(testmodelfile)){
			Resource r = new ResourceSetImpl().createResource(URI.createURI(testmodelfile.getPath()));
			r.load(in, Collections.EMPTY_MAP);
			return r;
		} catch (IOException e) {
			fail(e.getMessage());
			return null;
		}
	}
	
	
	
	/**
	 * Test will verify that all eClasses of a model are included in the generated contextMap.
	 */
	@Parameters(name = "{index}: {0}")
	public static Collection<String[]> initialize() {
		Collection<String[]> collection = new HashSet<>();
		
		String methodName = "createMapWithPackages";
		
		for (String modelName : new String[] {
				"bpmn2model.bpmn2", 
				"activitydiagram.uml",
				"classdiagram.uml",
				"componentdiagram.uml",
				"compositediagram.uml",
				"deploymentdiagram.uml",
				"sequencediagram.uml",
				"statemachinediagram.uml",
				"usecasediagram.uml"}) {
			collection.add(new String[]{modelName, methodName});
		}
		return collection;
	}

	@SuppressWarnings("unchecked")
	@Test
	public void createMapWithPackagesTest() {
		MultiOclChecker oclChecker = new MultiOclChecker();
		Resource model = loadModel(this.modelname);
		assertNotNull(model);
		
		
		final Method[] methods = oclChecker.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(this.methodname)) {
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
					logger.warning("Error message: " + e.getMessage());
					fail("Error during invoke (" + this.modelname + ")");
				}
				break;
			}
		}
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
}
