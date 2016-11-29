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

package carisma.modeltype.bpmn2.trace;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Locale;

import org.eclipse.bpmn2.Activity;
import org.eclipse.bpmn2.Process;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.junit.After;
import org.junit.Test;

import carisma.modeltype.bpmn2.util.Tupel;


/**
 * Test class.
 * 
 * @author Marcel Michel
 * 
 */
public class BPMN2TraceTest {

	/**
	 * Helper Class which should be tested.
	 */
	private BPMN2Trace traceHelper = new BPMN2Trace();

	/**
	 * The bpmn2 model loader.
	 */
	private ResourceSet rs = new ResourceSetImpl();

	/**
	 * The model resource.
	 */
	private Resource modelres = null;

	/**
	 * The relative filepath to the model directory.
	 */
	private String filepath = "resources/models/";
	
	/**
	 * Constant name for a Container.
	 */
	private static final String CONTAINER = "container";
	
	/**
	 * Error Message.
	 */
	private static final String ERROR_MSG = "Error: No Traces were calculated!";
	
	/**
	 * Method to load a bpmn2 model. The model will be stored in the modelres
	 * variable.
	 * 
	 * @param testmodelname
	 *            The model name
	 * @throws IOException 
	 */
	private void loadModel(final String testmodelname) throws IOException {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}
	
	/**
	 * Tests the allTracesBefore method.
	 * Test will load some model resources and will compare the defined result
	 * Testcases:
	 * -TracesAfterOk1: Task before and Task after are executed in the correct order: true
	 * -TraceAfterOk2: More complex example, but but the execution is still in the correct order: true
	 * -TracesAfterBad1: Similar to TracesAfterOk1 but the tasks are executed in the wrong order: false
	 * -TracesAfterBad2: More complex example, the execution is in the wrong order: false
	 * -TracesAfterBad3: Another complex example with a wrong order: false
	 * @throws IOException 
	 */
	@Test
	public final void allTracesBeforeTest() throws IOException {
		String activityBeforeName = "before";
		String activtyAfterName = "after";
		String containerName = CONTAINER;
		ArrayList<Tupel<String, Boolean>> testModels = new ArrayList<>();
		testModels.add(new Tupel<>("TracesAfterOk1.bpmn2", Boolean.TRUE));
		testModels.add(new Tupel<>("TracesAfterOk2.bpmn2", Boolean.TRUE));
		testModels.add(new Tupel<>("TracesAfterBad1.bpmn2", Boolean.FALSE));
		testModels.add(new Tupel<>("TracesAfterBad2.bpmn2", Boolean.FALSE));
		testModels.add(new Tupel<>("TracesAfterBad3.bpmn2", Boolean.FALSE));

		for (Tupel<String, Boolean> testModel : testModels) {
			loadModel(testModel.getO1());
			assertNotNull("model is null", this.modelres);

			Activity activityBefore = null, activityAfter = null;
			Process process = null;
			
			TreeIterator<EObject> iterator = this.modelres.getAllContents();
			while (iterator.hasNext()) {
				EObject obj = iterator.next();
				if (obj instanceof Activity) {
					if (((Activity) obj).getName().equalsIgnoreCase(activityBeforeName)) {
						activityBefore = (Activity) obj;
					} else if (((Activity) obj).getName().equalsIgnoreCase(activtyAfterName)) {
						activityAfter = (Activity) obj;
					}
				} else if (obj instanceof Process) {
					if (((Process) obj).getName().equalsIgnoreCase(containerName)) {
						process = (Process) obj;
					}
				}
			}
			assertNotNull("No \"activityBefore\" element is declared in the model", activityBefore);
			assertNotNull("No \"activityAfter\" element is declared in the model", activityAfter);
			assertNotNull("No process element found", process);

			assertTrue("Traces could not be calculated", this.traceHelper.calculateTraces(process));
			try {
				assertEquals("(" + testModel.getO1() + "): allTracesBeforeError", 
						Boolean.valueOf(this.traceHelper.allTracesBefore(activityBefore, activityAfter)),
						testModel.getO2());
			} catch (NoTracesCalculatedException e) {
				fail(ERROR_MSG);
			}
		}
	}

	/**
	 * Tests the allTracesInclude method.
	 * Test will load some model resources and will compare the defined result
	 * Testcases:
	 * -TracesIncludeOk1: Simple workflow with the markedActvity: true
	 * -TracesIncludeOk2: Complex worklflow with the markedActivity: true
	 * -TracesInlcudeBad1: Complex workflow with some traces that does not contain the markedActivity: false
	 * -TracesInlcudeBad2: Complex workflow with some traces that does not contain the markedActivity: false
	 * @throws IOException 
	 */
	@Test
	public final void allTracesIncludeTest() throws IOException {
		String activityName = "markedActivity";
		String containerName = CONTAINER;
		ArrayList<Tupel<String, Boolean>> testModels = new ArrayList<>();
		testModels.add(new Tupel<>("TracesIncludeOk1.bpmn2", Boolean.TRUE));
		testModels.add(new Tupel<>("TracesIncludeOk2.bpmn2", Boolean.TRUE));
		testModels.add(new Tupel<>("TracesIncludeBad1.bpmn2", Boolean.FALSE));
		testModels.add(new Tupel<>("TracesIncludeBad2.bpmn2", Boolean.FALSE));

		for (Tupel<String, Boolean> testModel : testModels) {
			loadModel(testModel.getO1());
			assertNotNull("model is null", this.modelres);

			Activity markedActivity = null;
			Process process = null;

			boolean foundMarkedActivity = false;
			TreeIterator<EObject> iterator = this.modelres.getAllContents();
			while (iterator.hasNext() && !foundMarkedActivity) {
				EObject obj = iterator.next();
				if (obj instanceof Activity && ((Activity) obj).getName().equalsIgnoreCase(activityName)) {
					markedActivity = (Activity) obj;
					foundMarkedActivity = true;
				} else if (obj instanceof Process) {
					if (((Process) obj).getName().equalsIgnoreCase(containerName)) {
						process = (Process) obj;
					}
				}
			}

			assertNotNull("The \"marked\" element is null", markedActivity);
			assertNotNull("No process element found", process);

			assertTrue("Traces could not be calculated", this.traceHelper.calculateTraces(process));
			try {
				assertEquals("(" + testModel + ") allTracesIncludeError", 
						Boolean.valueOf(this.traceHelper.allTracesInclude(markedActivity)),
						testModel.getO2());
			} catch (NoTracesCalculatedException e) {
				fail(ERROR_MSG);
			}
		}
	}

	/**
	 * Tests the hasTrace method.
	 * Test will load some resources and will create a trace with all tasks
	 * which matches the regExActivityName pattern.
	 * Testcases:
	 * -The imported trace exists in the workkflow: true
	 * -Some elements in the imported trace are swapped: false 
	 * @throws IOException 
	 */
	@Test
	public final void hasTraceTest1() throws IOException {
		String regExActivityName = "activityintrace\\d+";
		String containerName = CONTAINER;
		String[] testModels = new String[] { "HasTrace1.bpmn2" };

		for (String testModel : testModels) {
			loadModel(testModel);
			assertNotNull("model is null", this.modelres);

			ArrayList<Activity> activityList = new ArrayList<>();
			Process process = null;
			TreeIterator<EObject> iterator = this.modelres.getAllContents();
			while (iterator.hasNext()) {
				EObject obj = iterator.next();
				if (obj instanceof Activity && ((Activity) obj).getName().toLowerCase(Locale.ENGLISH).matches(regExActivityName)) {
					activityList.add((Activity) obj);
				} else if (obj instanceof Process && ((Process) obj).getName().equalsIgnoreCase(containerName)) {
					process = (Process) obj;
				}
			}

			assertTrue("No Activities with pattern " + regExActivityName + " found", 
					activityList.size() > 0);
			assertNotNull("No process element found", process);

			assertTrue("Traces could not be calculated", this.traceHelper.calculateTraces(process));
			try {
				assertTrue("(" + testModel + ") HasTraceError11: Trace not found", 
						this.traceHelper.hasTrace(activityList, null));
			} catch (NoTracesCalculatedException e2) {
				fail(ERROR_MSG);
			}

			// Swap some elements
			if (activityList.size() > 1) {
				Activity first = activityList.get(0);
				activityList.remove(0);
				Activity last = activityList.get(activityList.size() - 1);
				activityList.remove(activityList.size() - 1);
				activityList.add(0, last);
				activityList.add(first);
				try {
					assertFalse("(" + testModel + ") HasTraceError12: Trace should not exist", 
							this.traceHelper.hasTrace(activityList, null));
				} catch (NoTracesCalculatedException e1) {
					fail(ERROR_MSG);
				}

				// Undo operations
				activityList.remove(0);
				activityList.remove(activityList.size() - 1);
				activityList.add(last);
				activityList.add(0, first);
				try {
					assertTrue("(" + testModel + ") HasTraceError13: Trace not found", 
							this.traceHelper.hasTrace(activityList, null));
				} catch (NoTracesCalculatedException e) {
					fail(ERROR_MSG);
				}
			}

		}
	}

	/**
	 * Tests the hasTrace method.
	 * Test will load some model resources and will create a trace with the defined activities.
	 * Testcases:
	 * -The imported trace exists in the worflow: true
	 * -Some elements in the trace will be deleted, the result must be still a trace in the workflow:
	 * --Last Element: true
	 * --First Element: true
	 * --Element between the first and the last element: true
	 * @throws IOException 
	 */
	@Test
	public final void hasTraceTest2() throws IOException {
		String containerName = CONTAINER;
		String[] activityNames = { "Decide if normal post or special shipment",
				"Request quotes from carriers",
				"Assign a carrier and prepare paperwork",
				"Add paperwork and move package to pick area" };
		String[] testModels = new String[] { "HasTrace2.bpmn2" };

		for (String testModel : testModels) {
			loadModel(testModel);
			assertNotNull("model is null", this.modelres);

			ArrayList<Activity> activityList = new ArrayList<>();
			Process process = null;
			TreeIterator<EObject> iterator = this.modelres.getAllContents();
			while (iterator.hasNext()) {
				EObject obj = iterator.next();
				if (obj instanceof Activity) {
					for (int i = 0; i < activityNames.length; i++) {
						if (((Activity) obj).getName().toLowerCase(Locale.ENGLISH)
								.matches(activityNames[i].toLowerCase(Locale.ENGLISH))) {
							activityList.add((Activity) obj);
						}
					}
				} else if (obj instanceof Process && ((Process) obj).getName().equalsIgnoreCase(containerName)) {
					process = (Process) obj;
				}
			}

			assertNotNull("No process element found", process);
			assertSame("Not all defined activities could be found in the process", 
					Integer.valueOf(activityList.size()), 
					Integer.valueOf(activityNames.length));

			Activity removedElement = null;
			assertTrue("Traces could not be calculated", this.traceHelper.calculateTraces(process));
			try {
				assertTrue("(" + testModel + ") HasTraceError21: Trace not found", 
						this.traceHelper.hasTrace(activityList, null));
			} catch (NoTracesCalculatedException e) {
				fail(ERROR_MSG);
			}

			removedElement = activityList.remove(activityList.size() - 2);
			try {
				assertTrue("(" + testModel + ") HasTraceError22: Trace not found", 
						this.traceHelper.hasTrace(activityList, null));
			} catch (NoTracesCalculatedException e) {
				fail(ERROR_MSG);
			}
			activityList.add(activityList.size() - 1, removedElement);
			
			removedElement = activityList.remove(activityList.size() - 1);
			try {
				assertTrue("(" + testModel + ") HasTraceError23: Trace not found", 
						this.traceHelper.hasTrace(activityList, null));
			} catch (NoTracesCalculatedException e) {
				fail(ERROR_MSG);
			}
			activityList.add(removedElement);
			
			removedElement = activityList.remove(0);
			try {
				assertTrue("(" + testModel + ") HasTraceError24: Trace not found", 
						this.traceHelper.hasTrace(activityList, null));
			} catch (NoTracesCalculatedException e) {
				fail(ERROR_MSG);
			}
			activityList.add(0, removedElement);
		}
	}
	
	@After
	public void unloadModels(){
		for(Resource r : rs.getResources()){
			r.unload();
		}
	}
}