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
import java.util.List;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.junit.Test;

import carisma.check.bpmn2.ocl.Check;
import carisma.check.bpmn2.ocl.OclHelper;
import carisma.check.tests.tools.DebugHost;
import carisma.modeltype.bpmn2.BPMN2ModelLoader;
import carisma.ocl.library.LibraryFactory;
import carisma.ocl.library.OclExpression;


/**
 * Test class. 
 * @author Marcel Michel
 *
 */
public class CheckTest {

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
	private BPMN2ModelLoader ml = null;
	
	/**
	 * The bpmn2 model resource.
	 */
	private Resource modelres = null;
	
	/**
	 * Method to load a bpmn2 model.
	 * The model will be stored in the modelres variable.  
	 * @param testmodelname The model name
	 */
	public final void loadModel(final String testmodelname) {
		File testmodelfile = new File(filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		if (ml == null) {
			ml = new BPMN2ModelLoader();
		}
		try {
			modelres = ml.load(testmodelfile);
		} catch (IOException e) {
			fail(e.getMessage());
		}
	}
	
	/**
	 * Tests the correctness of the extraction procedure of the pattern definition. 
	 */
	@Test
	public final void regCheckPatternDefinition() {
		String methodName = "getPatternDefinition";

		final Method[] methods = check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					assertTrue("pattern1,pattern2".equals(
							(String) methods[i].invoke(check, 
									"<<pattern={pattern1,pattern2}>>")));

					assertTrue("pattern1".equals(
							(String) methods[i].invoke(check, 
									"<<pattern={pattern1}>>")));
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(-1, methodIndex);
	}
	
	/**
	 * Tests the correctness of the extraction procedure of the extension pattern.
	 */
	@Test
	public final void regCheckExtDefinition() {
		String methodName = "getExtDefinition";

		final Method[] methods = check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					assertTrue("extension.bpmn2extension".equals(
							(String) methods[i].invoke(check, 
									"<<ext=extension.bpmn2extension>>")));
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(-1, methodIndex);
	}
	
	/**
	 * Testing the regular expression isExtDefinition.
	 */
	@Test
	public final void regCheckIsExtDefinition() {
		String methodName = "isExtDefinition";

		final Method[] methods = check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(check, 
							"<<ext=extension.bpmn2extension>>")));
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(check, 
							"<<ext=a.b>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(check, 
							"<<ext=.bpmn2extension>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(check, 
							"<<ext=bpmn2extension>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(check, 
							"<<extension.bpmn2extension>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(check, 
							"ext=extension.bpmn2extension")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(check, 
							"extension.bpmn2extension")));
					
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(-1, methodIndex);
	}
	
	/**
	 * Testing the regular expression isPatternDefinition.
	 */
	@Test
	public final void regCheckIsPatternDefinition() {
		String methodName = "isPatternDefinition";

		final Method[] methods = check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(check, 
							"<<pattern={pattern1,pattern2}>>")));
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(check, 
							"<<pattern={1,2}>>")));
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(check, 
							"<<pattern={pattern1}>>")));
					assertTrue(Boolean.TRUE.equals(methods[i].invoke(check, 
							"<<pattern={*}>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(check, 
							"<<pattern={pattern1,}>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(check, 
							"<<pattern={*,pattern1}>>")));
					assertTrue(Boolean.FALSE.equals(methods[i].invoke(check, 
							"<<pattern=pattern1>>")));
					
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(-1, methodIndex);
	}
	
	/**
	 * Tests the correct selection of the getOclExpression method
	 * by adding dummy elements to a list, which should be afterwards filtered by the method. 
	 */
	@SuppressWarnings("unchecked")
	@Test
	public final void oclExpressions() {
		EList<OclExpression> inputList = new BasicEList<OclExpression>();
		
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
		final Method[] methods = check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					List<OclExpression> outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, "*");
					assertTrue(outputList.size() == 3);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					assertTrue(outputList.get(1).getName().equals(SECOND_EXPRESSION_NAME));
					assertTrue(outputList.get(2).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, "all");
					assertTrue(outputList.size() == 3);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					assertTrue(outputList.get(1).getName().equals(SECOND_EXPRESSION_NAME));
					assertTrue(outputList.get(2).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, "0");
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, "1");
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(SECOND_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, "2");
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, FIRST_EXPRESSION_NAME);
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, SECOND_EXPRESSION_NAME);
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(SECOND_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, THIRD_EXPRESSION_NAME);
					assertTrue(outputList.size() == 1);
					assertTrue(outputList.get(0).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, "0,third");
					assertTrue(outputList.size() == 2);
					assertTrue(outputList.get(0).getName().equals(FIRST_EXPRESSION_NAME));
					assertTrue(outputList.get(1).getName().equals(THIRD_EXPRESSION_NAME));
					
					outputList = (ArrayList<OclExpression>) methods[i].invoke(check, inputList, "third,0");
					assertTrue(outputList.size() == 2);
					assertTrue(outputList.get(0).getName().equals(THIRD_EXPRESSION_NAME));
					assertTrue(outputList.get(1).getName().equals(FIRST_EXPRESSION_NAME));
					
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(-1, methodIndex);
	}
	
	/**
	 * Test will perform two OCL queries with a given model.
	 */
	@Test
	public final void performQuery() {
		ArrayList<OclExpression> oclExpressions;
		
		OclExpression expr1 = LibraryFactory.eINSTANCE.createOclExpression();
		expr1.setContext("BaseElement");
		expr1.setQuery("true");
		
		OclExpression expr2 = LibraryFactory.eINSTANCE.createOclExpression();
		expr2.setContext("BaseElement");
		expr2.setQuery("false");
		
		loadModel("trade.bpmn2");
		assertNotNull(modelres);
		
		Field oclHelperField;
		try {
			oclHelperField = check.getClass().getDeclaredField("oclHelper");
			oclHelperField.setAccessible(true);
			oclHelperField.set(check, new OclHelper());
		} catch (Exception e) {
			fail(e.getMessage());
		}
		
		
		Field analysisHostField;
		try {
			analysisHostField = check.getClass().getDeclaredField("host");
			analysisHostField.setAccessible(true);
			analysisHostField.set(check, new DebugHost(false));
		} catch (Exception e) {
			fail(e.getMessage());
		}
		
		String methodName = "performQuery";
		final Method[] methods = check.getClass().getDeclaredMethods();
		int methodIndex = -1;
		for (int i = 0; i < methods.length; i++) {
			if (methods[i].getName().equals(methodName)) {
				methodIndex = i;
				methods[i].setAccessible(true);
				try {
					oclExpressions = new ArrayList<OclExpression>();
					oclExpressions.add(expr1);
					assertTrue(Boolean.TRUE.equals((Boolean) methods[i].invoke(check, modelres, oclExpressions)));
					oclExpressions.add(expr2);
					assertTrue(Boolean.FALSE.equals((Boolean) methods[i].invoke(check, modelres, oclExpressions)));
					
				} catch (Exception e) {
					fail(INVOKE_ERROR);
				}
				break;
			}
		}
		assertNotSame(-1, methodIndex);
	}
}
