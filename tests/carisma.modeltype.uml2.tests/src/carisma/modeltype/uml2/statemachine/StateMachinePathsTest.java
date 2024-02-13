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
package carisma.modeltype.uml2.statemachine;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.StateMachine;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.modeltype.uml2.TestHelper;
import carisma.modeltype.uml2.UMLHelper;

/**
 * this JUnit test-case tests the StateMachienPaths plugin.
 * @author Klaus Rudack
 *
 */
public class StateMachinePathsTest {
	
	/**
	 * path to the model-folder.
	 */
	private String filepath = "resources/models/stateMachinePath/";
	
	private Model model;
	
	
		
	/**
	 * this test tests if the plugin works correct if there is no path in a state-machine.
	 */
	@Test
	public final void testNoPath() {
		this.model = TestHelper.loadModel(this.filepath, "stateMachinePathsNoPath.uml");
		StateMachinePaths smp = new StateMachinePaths();
		List<StateMachine> smList = UMLHelper.getAllElementsOfType(this.model, StateMachine.class);
		assertEquals(1, smList.size());
		List<List<Element>> results = smp.getPaths(smList.get(0), null, false);
		assertNotNull(results);
		assertEquals(0, results.size());
	}
	
	/**
	 * this test tests if the plugin determines the correct path in a simple state-machine.
	 */
	@Test
	public final void testCorrect() {
		this.model = TestHelper.loadModel(this.filepath, "stateMachinePathsCorrect.uml");
		StateMachinePaths smp = new StateMachinePaths();
		List<StateMachine> smList = UMLHelper.getAllElementsOfType(this.model, StateMachine.class);
		assertEquals(1, smList.size());
		List<List<Element>> results = smp.getPaths(smList.get(0), null, false);
		assertNotNull(results);
		assertEquals(2, results.size());
	}
	
	/**
	 * this test tests if the plugin determines the correct path in a state-machine with parallel paths.
	 */
	@Test
	public final void testCorrectWithParallelization() {
		this.model = TestHelper.loadModel(this.filepath, "stateMachinePathsCorrectWithParallel.uml");
		StateMachinePaths smp = new StateMachinePaths();
		List<StateMachine> smList = UMLHelper.getAllElementsOfType(this.model, StateMachine.class);
		assertEquals(1, smList.size());
		List<List<Element>> results = smp.getPaths(smList.get(0), null, false);
		assertNotNull(results);
		assertEquals(12, results.size());
	}
	
	/**
	 * this test tests if the plugin throws a NullpointerException if the given StateMachien is null.
	 */
	@SuppressWarnings("static-method")
	@Test (expected = NullPointerException.class)
	public final void testNull() {
		StateMachinePaths smp = new StateMachinePaths();
		smp.getPaths(null, null, false);
	}
	
	
	/**
	 * this test tests if the plugin determines the correct path in a state-machine with a sub state.
	*/
	@Test
	@Ignore("substates are not handled properly")
	public final void testCorrectWithSubState() {
		//this.model = TestHelper.loadModel(this.filepath, "stateMachinePathsCorrectWithSubStates.uml");
		this.model = TestHelper.loadModel(this.filepath, "testSubStates.uml");
		StateMachinePaths smp = new StateMachinePaths();
		List<StateMachine> smList = UMLHelper.getAllElementsOfType(this.model, StateMachine.class);
		assertEquals(1, smList.size());
		List<List<Element>> results = smp.getPaths(smList.get(0), null, false);
		assertNotNull(results);
		assertEquals(7, results.size());
	}
	
	
//	KR: Funktioniert nicht
//	/**
//	 * this test tests if a shallow history  will be handled correctly.
//	 */
//	@Test
//	public final void testShallowHistory() {
//		loadModel("statemachinePahtsShallowHistory.uml");
//		assertTrue(modelres.getContents().size() > 0);
//		Model model = (Model) modelres.getContents().get(0);
//		assertNotNull(model);
//		StateMachinePaths smp = new StateMachinePaths();
//		List<StateMachine> smList = UMLHelper.getAllElementsOfType(model, StateMachine.class);
//		assertEquals(1, smList.size());
//		List<List<Element>> results = smp.getPaths(smList.get(0), null, false);
//		assertNotNull(results);
//		for (List<Element> list : results) {
//			print(list);
//		}
//		assertEquals(7, results.size());
//		modelres.unload();
//	}
	
//	KR: Funktioniert nicht
//	/**
//	 * this test tests if a SubState in a parallelization will handled proper.
//	 */
//	@Test
//	public final void testParallelAndSubState() {
//		loadModel("stateMachinePathsParallelAndSubState.uml");
//		assertTrue(modelres.getContents().size() > 0);
//		Model model = (Model) modelres.getContents().get(0);
//		assertNotNull(model);
//		StateMachinePaths smp = new StateMachinePaths();
//		List<StateMachine> smList = UMLHelper.getAllElementsOfType(model, StateMachine.class);
//		assertEquals(1, smList.size());
//		List<List<Element>> results = smp.getPaths(smList.get(0), null, false);
//		assertNotNull(results);
//		for (List<Element> list : results) {
//			print(list);
//		}
//		assertEquals(14, results.get(0).size());
//		assertEquals(6, results.size());
//		modelres.unload();
//	}
	
	
	/**
	 * this test tests if an empty SubState will handled proper.
	 */
	@Test
	public final void testEmptySubState() {
		this.model = TestHelper.loadModel(this.filepath, "stateMachinePathsEmptySubState.uml");
		StateMachinePaths smp = new StateMachinePaths();
		List<StateMachine> smList = UMLHelper.getAllElementsOfType(this.model, StateMachine.class);
		assertEquals(1, smList.size());
		List<List<Element>> results = smp.getPaths(smList.get(0), null, false);
		assertNotNull(results);
		assertEquals(2, results.size());
	}
	
	/**	
	 * unloads the model.
	 */
	@After
	public final void unload() {
		if (this.model != null) {
			TestHelper.unloadModel(this.model);
			this.model = null;
		}
	}
	
	/**
	 * Prints a given list to the console.
	 * @param list - list to print
	 *//*
	private void print(final List<Element> list) {
		if (list.size() > 0) {
			System.out.println("Beginning with path!");
			System.out.print(((NamedElement) list.get(0)).getName());
			for (int i = 1; i < list.size(); i++) {
				System.out.print("  -->  " + ((NamedElement) list.get(i)).getName());
			}
			System.out.println("\nFinished!");
		} else {
			System.out.println("Pfad ist leer");
		}
	}*/

}
