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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.junit.After;
import org.junit.Test;

import carisma.check.tests.tools.DebugHost;
import carisma.ocl.library.LibraryFactory;
import carisma.ocl.library.OclExpression;


import java.util.logging.Logger;
/**
 * Test class. 
 * @author Marcel Michel
 *
 */
public class CheckTest {

	private static final Logger logger = Logger.getLogger(CheckTest.class.getName());
	
	/**
	 * The Check Instance.
	 */
	private final Check check = new Check();
	
	/**
	 * Error message while invoking a method.
	 */
	private static final String INVOKE_ERROR = "Error during invoke";

    /**
     * Expression Name.
     */
    private static final String FIRST_EXPRESSION_NAME = "first";
    
    /**
     * Expression Name.
     */
    private static final String SECOND_EXPRESSION_NAME = "second";
    
    /**
     * Expression Name.
     */
    private static final String THIRD_EXPRESSION_NAME = "third";
	/**
	 * The path to the model directory. 
	 */
	private String filepath = "resources/models/";
	
	/**
	 * The bpmn2 model loader.
	 */
	private ResourceSet rs = new ResourceSetImpl();
	
	/**
	 * The bpmn2 model resource.
	 */
	private Resource modelres = null;
	
	/**
	 * Method to load a bpmn2 model.
	 * The model will be stored in the modelres variable.  
	 * @param testmodelname The model name
	 * @throws IOException 
	 */
	public final void loadModel(final String testmodelname) throws IOException {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}
	
	/**
	 * Tests the correctness of the extraction procedure of the pattern definition. 
	 */
	@Test
	public final void regCheckPatternDefinition() {
		String methodName = "getPatternDefinition";

		final Method[] methods = this.check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					assertTrue("pattern1,pattern2".equals(
							methods[i].invoke(this.check, 
									"<<pattern={pattern1,pattern2}>>")));

					assertTrue("pattern1".equals(
							methods[i].invoke(this.check, 
									"<<pattern={pattern1}>>")));
				} catch (Exception e) {
					logger.warning("Error message: " + e.getMessage());
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
	
	/**
	 * Tests the correctness of the extraction procedure of the extension pattern.
	 */
	@Test
	public final void regCheckExtDefinition() {
		String methodName = "getExtDefinition";

		final Method[] methods = this.check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					assertTrue("extension.bpmn2extension".equals(
							methods[i].invoke(this.check, 
									"<<ext=extension.bpmn2extension>>")));
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
	
	/**
	 * Testing the regular expression isExtDefinition.
	 */
	@Test
	public final void regCheckIsExtDefinition() {
		String methodName = "isExtDefinition";

		final Method[] methods = this.check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(this.check, 
							"<<ext=extension.bpmn2extension>>")));
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(this.check, 
							"<<ext=a.b>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, 
							"<<ext=.bpmn2extension>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, 
							"<<ext=bpmn2extension>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, 
							"<<extension.bpmn2extension>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, 
							"ext=extension.bpmn2extension")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, 
							"extension.bpmn2extension")));
					
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
	
	/**
	 * Testing the regular expression isPatternDefinition.
	 */
	@Test
	public final void regCheckIsPatternDefinition() {
		String methodName = "isPatternDefinition";

		final Method[] methods = this.check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(this.check, 
							"<<pattern={pattern1,pattern2}>>")));
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(this.check, 
							"<<pattern={1,2}>>")));
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(this.check, 
							"<<pattern={pattern1}>>")));
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(this.check, 
							"<<pattern={*}>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, 
							"<<pattern={pattern1,}>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, 
							"<<pattern={*,pattern1}>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, 
							"<<pattern=pattern1>>")));
					
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
	
	/**
	 * Tests the correct selection of the getOclExpression method
	 * by adding dummy elements to a list, which should be afterwards filtered by the method. 
	 */
	@SuppressWarnings("unchecked")
	@Test
	public final void oclExpressions() {
		EList<OclExpression> inputList = new BasicEList<>();
		
		OclExpression expr1 = LibraryFactory.eINSTANCE.createOclExpression();
		expr1.setName(FIRST_EXPRESSION_NAME);
		OclExpression expr2 = LibraryFactory.eINSTANCE.createOclExpression();
		expr2.setName(SECOND_EXPRESSION_NAME);
		OclExpression expr3 = LibraryFactory.eINSTANCE.createOclExpression();
		expr3.setName(THIRD_EXPRESSION_NAME);
		
		inputList.add(expr1);
		inputList.add(expr2);
		inputList.add(expr3);
		
		String methodName = "getOclExpression";
		final Method[] methods = this.check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					List<OclExpression> outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, "*");
					assertTrue(outputList.size() == 3);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					assertTrue(outputList.get(1).getName().equals(SECOND_EXPRESSION_NAME));
					assertTrue(outputList.get(2).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, "all");
					assertTrue(outputList.size() == 3);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					assertTrue(outputList.get(1).getName().equals(SECOND_EXPRESSION_NAME));
					assertTrue(outputList.get(2).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, "0");
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, "1");
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(SECOND_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, "2");
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, FIRST_EXPRESSION_NAME);
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, SECOND_EXPRESSION_NAME);
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(SECOND_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, THIRD_EXPRESSION_NAME);
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, "0,third");
					assertTrue(outputList.size() == 2);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					assertTrue(outputList.get(1).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(this.check, inputList, "third,0");
					assertTrue(outputList.size() == 2);
					assertTrue(outputList.get(0).getName().equals(THIRD_EXPRESSION_NAME));
					assertTrue(outputList.get(1).getName().equals(FIRST_EXPRESSION_NAME));
					
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
	
	/**
	 * Test will perform two OCL queries with a given model.
	 * @throws IOException 
	 */
	@Test
	public final void performQuery() throws IOException {
		ArrayList<OclExpression> oclExpressions;
		
		OclExpression expr1 = LibraryFactory.eINSTANCE.createOclExpression();
		expr1.setContext("BaseElement");
		expr1.setQuery("true");
		
		OclExpression expr2 = LibraryFactory.eINSTANCE.createOclExpression();
		expr2.setContext("BaseElement");
		expr2.setQuery("false");
		
		loadModel("trade.bpmn2");
		assertNotNull(this.modelres);
		
		Field oclHelperField;
		try {
			oclHelperField = this.check.getClass().getDeclaredField("oclHelper");
			oclHelperField.setAccessible(true);
			oclHelperField.set(this.check, new OclHelper());
		} catch (Exception e) {
			fail(e.getMessage());
		}
		
		
		Field analysisHostField;
		try {
			analysisHostField = this.check.getClass().getDeclaredField("host");
			analysisHostField.setAccessible(true);
			analysisHostField.set(this.check, new DebugHost(false));
		} catch (Exception e) {
			fail(e.getMessage());
		}
		
		String methodName = "performQuery";
		final Method[] methods = this.check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					oclExpressions = new ArrayList<>();
					oclExpressions.add(expr1);
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(this.check, this.modelres, oclExpressions)));
					oclExpressions.add(expr2);
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(this.check, this.modelres, oclExpressions)));
					
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(Integer.valueOf(-1), Integer.valueOf(methodIndex));
	}
	
	@After
	public void unloadModels(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
