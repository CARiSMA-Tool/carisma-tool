/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {INITIAL AUTHOR} - initial API and implementation and/or initial documentation
 *******************************************************************************/

package carisma.check.bpmn2.ocl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Locale;

import org.eclipse.bpmn2.Bpmn2Package;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.junit.Test;

import carisma.check.tests.tools.Tupel;
import carisma.modeltype.bpmn2.extended.ExtendedPackage;
import carisma.modeltype.bpmn2.extension.ExtensionPackage;
import carisma.ocl.library.OclLibrary;


/**
 * Test class.
 * @author Marcel Michel
 *
 */
public class OclHelperTest {

	/**
	 * The path to the model directory. 
	 */
	private String filepath = "resources/models/";
	
	/**
	 * Tests the correct mapping.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void extendedContext() {
		OclHelper oclHelper = new OclHelper();
		assertNull(oclHelper.getExtendedContextClass("ThisClassShouldNotExist"));

		ArrayList<Tupel<EClass, String[]>> testCases = new ArrayList<>();

		testCases.add(new Tupel<>(
				ExtendedPackage.eINSTANCE.getExtendedLane(), new String[] {
						"lane", "Lane", "LANE" }));

		testCases.add(new Tupel<>(
				ExtendedPackage.eINSTANCE.getExtendedTask(), new String[] {
						"task", "Task", "TASK" }));

		testCases.add(new Tupel<>(
				ExtendedPackage.eINSTANCE.getExtendedProcess(),
				new String[] { "process", "Process", "PROCESS" }));

		testCases.add(new Tupel<>(
				ExtensionPackage.eINSTANCE.getSelection(),
				new String[] { "selection", "Selection", "SELECTION" }));

		testCases.add(new Tupel<>(
				ExtensionPackage.eINSTANCE.getTaskSet(), new String[] {
						"taskset", "taskSet", "TaskSet", "TASKSET" }));

		for (Tupel<EClass, String[]> testcase : testCases) {
			for (String className : testcase.getO2()) {
				assertEquals(testcase.getO1(),
						oclHelper.getExtendedContextClass(className));
			}
		}
	}

	/**
	 * Test will try to resolve each content class of every bpmn2 element.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void contextClass() {
		OclHelper oclHelper = new OclHelper();

		assertNull(oclHelper.getExtendedContextClass("ThisClassShouldNotExist"));

		for (EObject obj : Bpmn2Package.eINSTANCE.eContents()) {
			if (obj instanceof EClass) {
				assertEquals(obj,
						oclHelper.getContextClass(((EClass) obj).getName()));
				assertEquals(obj, oclHelper.getContextClass(((EClass) obj)
						.getName().toUpperCase(Locale.ENGLISH)));
				assertEquals(obj, oclHelper.getContextClass(((EClass) obj)
						.getName().toLowerCase(Locale.ENGLISH)));
			}
		}
	}
	
	/**
	 * Test will load a dummy OCL Library and will compare the entries.
	 */
	@Test 
	public final void loadOclLibrary() {
		String testmodelfilename = "testLibrary.col";
		File testmodelfile = new File(this.filepath + File.separator + testmodelfilename);
		assertTrue(testmodelfile.exists());
		
		OclLibrary testLib = null;
		try {
			testLib = OclHelper.getOclLibrary(testmodelfile);
		} catch (IOException e) {
			fail(e.getMessage());
		}
		
		assertTrue(testLib != null);
		assertTrue(testLib.getName().equals("Library"));
		
		assertTrue(testLib.getOclExpressions().get(0).getContext().equals("context"));
		assertTrue(testLib.getOclExpressions().get(0).getDescription().equals("description"));
		assertTrue(testLib.getOclExpressions().get(0).getName().equals("name"));
		assertTrue(testLib.getOclExpressions().get(0).getQuery().equals("query"));
	}
}
