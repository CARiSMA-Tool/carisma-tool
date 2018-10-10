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
package carisma.check.bpmn2.marisk;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.eclipse.bpmn2.Activity;
import org.eclipse.bpmn2.Bpmn2Factory;
import org.eclipse.bpmn2.CancelEventDefinition;
import org.eclipse.bpmn2.ErrorEventDefinition;
import org.eclipse.bpmn2.EscalationEventDefinition;
import org.eclipse.bpmn2.FlowElementsContainer;
import org.eclipse.bpmn2.Lane;
import org.eclipse.bpmn2.Process;
import org.eclipse.bpmn2.SignalEventDefinition;
import org.eclipse.bpmn2.Task;
import org.eclipse.bpmn2.TimerEventDefinition;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.junit.Test;

import carisma.bpmn2.marisk.util.IncompleteMappingExeption;
import carisma.bpmn2.marisk.util.MariskHelper;
import carisma.check.bpmn2.marisk.util.Tupel;

public class MariskHelperTest {

	/**
	 * Bpmn2 factory.
	 */
	private Bpmn2Factory factory = Bpmn2Factory.eINSTANCE;
	
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
	 * Method to load a bpmn2 model. The model will be stored in the modelres
	 * variable.
	 * 
	 * @param testmodelname
	 *            The model name
	 */
	private void loadModel(final String testmodelname) {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		try (FileInputStream in = new FileInputStream(testmodelfile)){
			this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getPath()));
			this.modelres.load(in, Collections.EMPTY_MAP);
		} catch (IOException e) {
			fail(e.getMessage());
		}
	}
	
	/**
	 * Returns the name of an object.
	 * @param obj object
	 * @return the name of the object
	 */
	private static String getName(final EObject obj) {
		if (obj != null) {
			EStructuralFeature sf = obj.eClass().getEStructuralFeature("name");
			if (sf == null) {
				sf = obj.eClass().getEStructuralFeature("Name");
			}
			if (sf == null) {
				sf = obj.eClass().getEStructuralFeature("NAME");
			}
			if (sf != null) {
				return String.valueOf(obj.eGet(sf));
			}
			return "[unnamed " + obj.eClass().getName() + "]";
		}
		return "[no element]";
	}
	
	/**
	 * Tests the extractActivity Method.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void extractActivitiesTest() {
		List<Tupel<String, String[]>> testList = new ArrayList<>();
		testList.add(new Tupel<>(
				"Activity1,Activity2",
				new String[] {"Activity1", "Activity2"}));
		testList.add(new Tupel<>(
				"Activity1, Activity2",
				new String[] {"Activity1", "Activity2"}));
		testList.add(new Tupel<>(
				"Activity1 ,Activity2",
				new String[] {"Activity1", "Activity2"}));
		testList.add(new Tupel<>(
				"Activity1 , Activity2",
				new String[] {"Activity1", "Activity2"}));
		testList.add(new Tupel<>(
				"Activity 1 , Activity 2",
				new String[] {"Activity 1", "Activity 2"}));
		testList.add(new Tupel<>(
				"Activity1",
				new String[] {"Activity1"}));
		testList.add(new Tupel<>(
				"Activity1 , Activity2 , Activity3",
				new String[] {"Activity1", "Activity2", "Activity3",}));
		
		for (Tupel<String, String[]> testEntry : testList) {
			String[] tmp = MariskHelper.extractActivities(testEntry.getO1());
			if (tmp.length != testEntry.getO2().length) {
				fail("ExtractActivities: Invalid array size");
			}
			for (int i = 0; i < tmp.length; i++) {
				assertEquals(testEntry.getO2()[i], tmp[i]);
			}
		}
	}
	
	/**
	 * Tests the parseActivities method.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void parseActivitiesTest() {
		List<Tupel<String, Boolean>> testList = new ArrayList<>();
		testList.add(new Tupel<> (
				"Activity1,Activity2", Boolean.TRUE));
		testList.add(new Tupel<> (
				"Activity 1 , Activity 2", Boolean.TRUE));
		testList.add(new Tupel<> (
				"Activity1", Boolean.TRUE));
		testList.add(new Tupel<> (
				"Activity 1", Boolean.TRUE));
		testList.add(new Tupel<> (
				"Activity1,Activity2,", Boolean.FALSE));
		testList.add(new Tupel<> (
				",Activity1,Activity2", Boolean.FALSE));
		testList.add(new Tupel<> (
				"", Boolean.FALSE));
		testList.add(new Tupel<> (
				",", Boolean.FALSE));
		
		for (Tupel<String, Boolean> testEntry : testList) {
			assertEquals(Boolean.valueOf(MariskHelper.parseActivities(testEntry.getO1())), testEntry.getO2());
		}
	}
	
	/**
	 * Tests the getUnmachtedActivities method.
	 */
	@Test
	public final void getUnmatchedActivitiesTest() {
		
		Activity act1 = this.factory.createTask();
		act1.setName("act1");
		Activity act2 = this.factory.createTask();
		act2.setName("act2");
		Activity act3 = this.factory.createTask();
		act3.setName("act3");
		
		List<Activity> testList = new ArrayList<>();
		testList.add(act1);
		testList.add(act2);
		testList.add(act3);
		
		String[] activityNames = {"act1", "act2", "act3"};
		
		List<String> result = null;
		
		result = MariskHelper.getUnmatchedActivities(activityNames, testList);
		assertEquals(0, result.size());
		
		testList.remove(act1);
		result = MariskHelper.getUnmatchedActivities(activityNames, testList);
		assertEquals(1, result.size());
		assertTrue(result.get(0).equalsIgnoreCase(act1.getName()));
		testList.add(0,act1);
		
		testList.remove(act3);
		result = MariskHelper.getUnmatchedActivities(activityNames, testList);
		assertEquals(1, result.size());
		assertTrue(result.get(0).equalsIgnoreCase(act3.getName()));
		testList.add(act3);
		
		testList.remove(act2);
		result = MariskHelper.getUnmatchedActivities(activityNames, testList);
		assertEquals(1, result.size());
		assertTrue(result.get(0).equalsIgnoreCase(act2.getName()));
		testList.add(1,act2);

		testList.remove(act1);
		testList.remove(act2);
		result = MariskHelper.getUnmatchedActivities(activityNames, testList);
		assertEquals(2, result.size());
		assertTrue(result.get(0).equalsIgnoreCase(act1.getName()) ||
				result.get(1).equalsIgnoreCase(act1.getName()));
		assertTrue(result.get(0).equalsIgnoreCase(act2.getName()) ||
				result.get(1).equalsIgnoreCase(act2.getName()));
		
		testList.remove(act3);
		result = MariskHelper.getUnmatchedActivities(activityNames, testList);
		assertEquals(3, result.size());
		assertTrue(result.get(0).equalsIgnoreCase(act1.getName()) ||
				result.get(1).equalsIgnoreCase(act1.getName()) || 
				result.get(2).equalsIgnoreCase(act1.getName()));
		assertTrue(result.get(0).equalsIgnoreCase(act2.getName()) ||
				result.get(1).equalsIgnoreCase(act2.getName()) || 
				result.get(2).equalsIgnoreCase(act2.getName()));
		assertTrue(result.get(0).equalsIgnoreCase(act3.getName()) ||
				result.get(1).equalsIgnoreCase(act3.getName()) || 
				result.get(2).equalsIgnoreCase(act3.getName()));
	}
	
	/**
	 * Tests the mapActivities method.
	 * @throws IncompleteMappingExeption 
	 */
	@Test
	public final void mapActivitiesTest() throws IncompleteMappingExeption {
		loadModel("MapActivitiesContainer.bpmn2");
		assertNotNull(this.modelres);
		
		try	{
			List<Activity> result = MariskHelper.mapActivities(this.modelres, new String[] {"act1"});
			assertEquals(1, result.size());
			assertTrue(result.get(0).getName().equalsIgnoreCase("act1"));
		}
		catch(IncompleteMappingExeption e){
			fail();
		}
		
		try {
			MariskHelper.mapActivities(this.modelres, new String[] {"act1", "notAvailable"});
			fail();
		}
		catch(IncompleteMappingExeption e){
			// Exception is expected
		}
		
		try {
			MariskHelper.mapActivities(this.modelres, new String[] {"notAvailable", "act1"});
			fail();
		}
		catch(IncompleteMappingExeption e){
			// Exception is expected
		}
		
		try {
			List<Activity> result = MariskHelper.mapActivities(this.modelres, new String[] {"act1", "act2", "act3"});
			assertEquals(3, result.size());
			assertTrue(result.get(0).getName().equalsIgnoreCase("act1") ||
					result.get(1).getName().equalsIgnoreCase("act1") || 
					result.get(2).getName().equalsIgnoreCase("act1"));
			assertTrue(result.get(0).getName().equalsIgnoreCase("act2") ||
					result.get(1).getName().equalsIgnoreCase("act2") || 
					result.get(2).getName().equalsIgnoreCase("act2"));
			assertTrue(result.get(0).getName().equalsIgnoreCase("act3") ||
					result.get(1).getName().equalsIgnoreCase("act3") || 
					result.get(2).getName().equalsIgnoreCase("act3"));
		}
		catch(IncompleteMappingExeption e){
			fail();
		}
		
	}

	/**
	 * Tests the mapContainer method.
	 */
	@Test
	public final void mapContainerTest() {
		loadModel("MapActivitiesContainer.bpmn2");
		assertNotNull(this.modelres);
		
		FlowElementsContainer container = MariskHelper.mapContainer(this.modelres, "InvalidContainerName");
		assertNull(container);
		container = MariskHelper.mapContainer(this.modelres, "Process for Pool nr 1");
		assertNotNull(container);
		assertTrue(getName(container).equals("Process for Pool nr 1"));
	}
	
	/**
	 * Checks if each class of a given model is resolved correctly by name.
	 */
	@Test
	public final void findContextInPackageTest() {
		for (String modelname : new String[] {"TracesAfterOk1.bpmn2"}) {
			loadModel(modelname);
			assertNotNull(this.modelres);
			
			TreeIterator<EObject> iterator = this.modelres.getAllContents();
			while (iterator.hasNext()) {
				EObject obj = iterator.next();
				assertEquals(obj.eClass().getName(), 
						MariskHelper.findEClass(obj.eClass().getName(), this.modelres).getName());
			}
		}
	}
	
	/**
	 * Tests the separation of duty method.
	 * Testcases with two two task elements t1 and t2:
	 * -t1 and t2 are not in any lane: true
	 * -t1 is in one lane l1 and t2 is not in any line: true
	 * -t1 and t2 are in lane l1: false
	 * -t1 is in lane l1 and t2 is in lane l2: true
	 */
	@Test
	public final void checkDutySeparationTest() {
		Task t1 = this.factory.createTask();
		Task t2 = this.factory.createTask();

		assertTrue("SoD1: Tasks t1 and t2 are separated " +
				"(should not violate the SoD constraint)", 
				MariskHelper.checkSeparationOfDuty(t1, t2));

		Lane l1 = this.factory.createLane();
		Lane l2 = this.factory.createLane();

		l1.getFlowNodeRefs().add(t1);
		assertTrue("SoD2: Tasks t1 and t2 are separated " +
				"(should not violate the SoD constraint)", 
				MariskHelper.checkSeparationOfDuty(t1, t2));

		l1.getFlowNodeRefs().add(t2);
		assertFalse("SoD3: Tasks t1 and t2 are in the same lane " +
				"(SoD constraint should be violated)", 
				MariskHelper.checkSeparationOfDuty(t1, t2));

		l1.getFlowNodeRefs().remove(t2);
		l2.getFlowNodeRefs().add(t2);
		assertTrue("SoD4: Tasks t1 and t2 are not in the same lane " +
				"(should not violate the SoD constraint)", 
				MariskHelper.checkSeparationOfDuty(t1, t2));
	}
	
	/**
	 * Tests the hasBoundaryEventOfType method.
	 * Test will load some model resources and will compare the output of the 
	 * hasBoundaryEventOfType with given examples.
	 * Testcases:
	 * -Expected[CancleActivity, ClassA], Actual[CancleActivity, ClassA]: true
	 * -Expected[NonCancleActivity, ClassA], Actual[NonCancleActivity, ClassA]: true
	 * -Expected[NonCancleActivity, ClassA], Actual[CancleActivity, ClassA]: false
	 * -Expected[CancleActivity, ClassA], Actual[NonCancleActivity, ClassA]: false
	 * -Expected[CancleActivity, ClassB], Actual[CancleActivity, ClassA]: false
	 * -Expected[NonCancleActivity, ClassB], Actual[NonCancleActivity, ClassA]: false
	 * -Expected[CancleActivity, ClassB], Actual[NonCancleActivity, ClassA]: true
	 */
	@Test
	public final void hasBoundaryEventOfTypeTest() {
		String containerName = CONTAINER;
		String escalationEvent = "EscalationEvent";
		String timerEvent = "TimerEvent";
		String signalEvent = "SignalEvent";
		String errorEvent = "ErrorEvent";
		String[] activityNames = new String[] {escalationEvent,
				timerEvent,
				signalEvent,
				errorEvent
		};
		String[] testModels = new String[] {"HasBoundaryEventOfType.bpmn2"};
		
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
				} else if (obj instanceof Process 
				        && ((Process) obj).getName().toLowerCase(Locale.ENGLISH).equals(containerName)) {
					process = (Process) obj;
				}
			}
			
			assertNotNull("No process element found", process);
			assertTrue("Not all defined activities could be found in the process", 
					activityList.size() == activityNames.length);
			
			for (Activity a : activityList) {
				if (a.getName().equalsIgnoreCase(escalationEvent)) {
					//This Event is a cancel activity
					assertTrue("(" + testModel + ") EscaltionEvent1 error", MariskHelper.hasBoundaryEventOfType(a, EscalationEventDefinition.class, true));
					assertFalse("(" + testModel + ") EscaltionEvent2 error", MariskHelper.hasBoundaryEventOfType(a, EscalationEventDefinition.class, false));
					assertFalse("(" + testModel + ") EscaltionEvent3 error", MariskHelper.hasBoundaryEventOfType(a, TimerEventDefinition.class, true));
					assertFalse("(" + testModel + ") EscaltionEvent4 error", MariskHelper.hasBoundaryEventOfType(a, TimerEventDefinition.class, false));
				} else if (a.getName().equalsIgnoreCase(timerEvent)) {
					//This Event is a cancel activity
					assertTrue("(" + testModel + ") TimerEvent1 error", MariskHelper.hasBoundaryEventOfType(a, TimerEventDefinition.class, true));
					assertFalse("(" + testModel + ") TimerEvent2 error", MariskHelper.hasBoundaryEventOfType(a, TimerEventDefinition.class, false));
					assertFalse("(" + testModel + ") TimerEvent3 error", MariskHelper.hasBoundaryEventOfType(a, SignalEventDefinition.class, true));
					assertFalse("(" + testModel + ") TimerEvent4 error", MariskHelper.hasBoundaryEventOfType(a, SignalEventDefinition.class, false));
				} else if (a.getName().equalsIgnoreCase(signalEvent)) {
					//This Event is not a cancel activity
					assertFalse("(" + testModel + ") SignalEvent1 error", MariskHelper.hasBoundaryEventOfType(a, SignalEventDefinition.class, true));
					assertTrue("(" + testModel + ") SignalEvent2 error", MariskHelper.hasBoundaryEventOfType(a, SignalEventDefinition.class, false));
					assertFalse("(" + testModel + ") SignalEvent3 error", MariskHelper.hasBoundaryEventOfType(a, ErrorEventDefinition.class, false));
					assertFalse("(" + testModel + ") SignalEvent4 error", MariskHelper.hasBoundaryEventOfType(a, ErrorEventDefinition.class, true));
				} else if (a.getName().equalsIgnoreCase(errorEvent)) {
					//This Event is not a cancel activity
					assertFalse("(" + testModel + ") ErrorEvent1 error", MariskHelper.hasBoundaryEventOfType(a, ErrorEventDefinition.class, true));
					assertTrue("(" + testModel + ") ErrorEvent2 error", MariskHelper.hasBoundaryEventOfType(a, ErrorEventDefinition.class, false));
					assertFalse("(" + testModel + ") ErrorEvent3 error", MariskHelper.hasBoundaryEventOfType(a, CancelEventDefinition.class, false));
					assertFalse("(" + testModel + ") ErrorEvent4 error", MariskHelper.hasBoundaryEventOfType(a, CancelEventDefinition.class, true));
				}
			}
		}
	}
}
