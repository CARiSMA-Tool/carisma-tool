package carisma.tool.evolution;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EOperation;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.ChangeConstraint;
import carisma.evolution.ConstraintType;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.DeltaFactory;
import carisma.evolution.SubstElement;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;


/** Test class for the DeltaFactory.
 * 
 * @author bberghoff
 *
 */
public class DeltaFactoryTest {
	
	/**
	 * Constant name for the first Change.
	 */
	private static final String FIRST_CHANGE = "FirstChange";
	
	/**
	 * Constant name for the second Change.
	 */
	private static final String SECOND_CHANGE = "SecondChange";
	
	/**
	 * Constant name for the third Change.
	 */
	private static final String THIRD_CHANGE = "ThirdChange";
	
	/**
	 * Constant name for the fourth Change.
	 */
	private static final String FOURTH_CHANGE = "FourthChange";

	/**
	 * Constant name for the first Alternative of any Change.
	 */
	private static final String ALTERNATIVE_ONE =  " (Alt.1)";

	/**
	 * Constant name for the second Alternative of any Change.
	 */
	private static final String ALTERNATIVE_TWO =  " (Alt.2)";
	
	/**
	 * Constant name of a TargetMock used for DelElements.
	 */
	private static final String DELE_TARGET_NAME = "delEle";
	
	/**
	 * Constant name of the field 'andDependencies' in 'DeltaFactory'.
	 */
	private static final String AND_DEPENDENCIES = "andDependencies";
	
	/**
	 * Constant name of the field 'notDependencies' in 'DeltaFactory'.
	 */
	private static final String NOT_DEPENDENCIES = "notDependencies";
			
	/**
	 * Constant for the field changes in DeltaFactory.
	 */
	private static final String FIELD_CHANGES = "changes";
	
	/**
	 * Constant for the name of the method 'parseConstraints()'.
	 */
	private static final String PARSE_CONSTRAINTS = "parseConstraints";
	
	/**
	 * Constant for the name of the method PROCESS_CONSTRAINTS.
	 */
	private static final String PROCESS_CONSTRAINTS = "processConstraints";
	
	/**
	 * Constant for the field reqDependencies in DeltaFactory.
	 */
	private static final String FIELD_REQ_DEPENDENCIES = "reqDependencies";
	
	/** 
	 * Method DeltaFactory.hasCycles(Map<Change, Set<Change>>).
	 */
	private static Method hasCycles = null;
	
	/** Initializing Method. 
	 */
	@BeforeClass
	public static void init() {

		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			hasCycles = deltaFactoryClass.getDeclaredMethod("hasCycles", Map.class);
			hasCycles.setAccessible(true);
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	
	/** Test the private method hasCycles().
	 *  Input is an empty Map<Change, Set<Change>> .
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void hasCycleEmptyInputTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		try {
			assertFalse(((Boolean) hasCycles.invoke(deltaFact, new HashMap<Change, Set<Change>>())).booleanValue());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	
	/** Test the private method hasCycles().
	 * FIRST_CHANGE  <-> SECOND_CHANGE
	 * SECOND_CHANGE <-> THIRD_CHANGE
	 * THIRD_CHANGE  <-> FOURTH_CHANGE
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void hasCycleChainTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		try {
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			Change fourth = new Change(FOURTH_CHANGE);
			Map<Change, Set<Change>> andDependencies = new HashMap<>();
			andDependencies.put(first, new HashSet<Change>());
			andDependencies.put(second, new HashSet<Change>());
			andDependencies.put(third, new HashSet<Change>());
			andDependencies.put(fourth, new HashSet<Change>());
			andDependencies.get(first).add(second);
			andDependencies.get(second).add(third);
			andDependencies.get(third).add(fourth);
			
			assertFalse(((Boolean) hasCycles.invoke(deltaFact, andDependencies)).booleanValue());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	/** Tests the private method hasCycles().
	 * Input are three changes with the following dependencies.
	 * FirstChange requires SecondChange
	 * FirstChange requires ThirdChange
	 * ThirdChange requires FirstChange
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void hasCycleMultipleRequirementsTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		
		Change first = new Change(FIRST_CHANGE);
		Change second = new Change(SECOND_CHANGE);
		Change third = new Change(THIRD_CHANGE);
		
		Map<Change, Set<Change>> andDependencies = new HashMap<>();
		andDependencies.put(first, new HashSet<Change>());
		andDependencies.put(second, new HashSet<Change>());
		andDependencies.put(third, new HashSet<Change>());

		andDependencies.get(first).add(second);
		andDependencies.get(first).add(third);
		andDependencies.get(second).add(first);
		try {
			assertTrue(((Boolean) hasCycles.invoke(deltaFact, andDependencies)).booleanValue());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	
	/** Test the private method hasCycles().
	 * Cycle.
	 * FristChange requires First
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void hasCycleSimpleCycleTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		try {
			Change first = new Change(FIRST_CHANGE);
			Map<Change, Set<Change>> andDependencies = new HashMap<>();
			andDependencies.put(first, new HashSet<Change>());
			andDependencies.get(first).add(first);
	
			assertTrue(((Boolean) hasCycles.invoke(deltaFact, andDependencies)).booleanValue());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	
	/** Test the private method hasCycles().
	 * Cycle.
	 * FristChange requires SecondChange.
	 * SecondChange requires ThirdChange.
	 * ThirdChange requires FourthChange.
	 * FourthChange requires FirstChange.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void hasCycleTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		try {
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			Change fourth = new Change(FOURTH_CHANGE);
			Map<Change, Set<Change>> andDependencies = new HashMap<>();
			andDependencies.put(first, new HashSet<Change>());
			andDependencies.put(second, new HashSet<Change>());
			andDependencies.put(third, new HashSet<Change>());
			andDependencies.put(fourth, new HashSet<Change>());
			andDependencies.get(first).add(second);
			andDependencies.get(second).add(third);
			andDependencies.get(third).add(fourth);
			andDependencies.get(fourth).add(first);
			
			assertTrue(((Boolean) hasCycles.invoke(deltaFact, andDependencies)).booleanValue());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	
	/** Test the private method computeMaxNumberOfDeltas.
	 *  Expected return value is an empty List of Changes.
	 */
	@SuppressWarnings("static-method")
	@Test 
	public final void computeMaxNumberOfDeltasEmptyList() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			Method method = deltaFactoryClass.getDeclaredMethod("computeMaxNumberOfDeltas", List.class);
			method.setAccessible(true);
			List<Change> changes = new ArrayList<>();
			
			Integer number = (Integer) method.invoke(deltaFact, changes);
			assertEquals(0,  number.intValue());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	

	/** Test the private method computeMaxNumberOfDeltas.
	 *  3 Changes: two with 4 alternatives and one with 1 alternative.
	 */
	@SuppressWarnings("static-method")
	@Test 
	public final void computeMaxNumberOfDeltasInputList() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			Method method = deltaFactoryClass.getDeclaredMethod("computeMaxNumberOfDeltas", List.class);
			method.setAccessible(true);
			
			//---------Initializing test-data------------------//
			List<Change> changes = new ArrayList<>();
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);

			Alternative firstA = new Alternative();
			Alternative firstB = new Alternative();
			Alternative firstC = new Alternative();
			Alternative firstD = new Alternative();
			
			first.addAlternative(firstA);
			first.addAlternative(firstB);
			first.addAlternative(firstC);
			first.addAlternative(firstD);

			Alternative secondA = new Alternative();
			Alternative secondB = new Alternative();
			Alternative secondC = new Alternative();
			Alternative secondD = new Alternative();

			second.addAlternative(secondA);
			second.addAlternative(secondB);
			second.addAlternative(secondC);
			second.addAlternative(secondD);

			Alternative thirdA = new Alternative();
			
			third.addAlternative(thirdA);
			
			changes.add(first);
			changes.add(second);
			changes.add(third);
			//-----------------------Finished initializing test-data-----------------//
			
			int maxNumberOfDeltas = 50;
			
			assertEquals(maxNumberOfDeltas, ((Integer) method.invoke(deltaFact, changes)).intValue());
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	
	/** Tests the private method processConstraints.
	 * Input: Empty List 
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void processConstraintsEmptyList() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			List<Delta> deltas = new ArrayList<>();
			Method method = deltaFactoryClass.getDeclaredMethod(PROCESS_CONSTRAINTS, List.class);
			method.setAccessible(true);
			method.invoke(deltaFact, deltas);
			assertTrue(deltas.isEmpty());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "Couln't find method '" + "processConstraints" + "'", e);
		}
	}
	
	/** Tests the private method processConstraints.
	 * Input: List contains one AND Constraint
	 * Return: empty List since the one Delta is not in compliance with the Constraint.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void processConstraintsSimpleANDListFail() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {

			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			
			List<String> changeIDList = new ArrayList<>();
			changeIDList.add(first.getRef());
			
			DelElement delEle = new DelElement(new TargetMock(DELE_TARGET_NAME));
			List<DeltaElement> deltaElements = new ArrayList<>();
			deltaElements.add(delEle);
			Delta fail = new Delta(changeIDList, deltaElements);
			
			Field andDependencies = deltaFactoryClass.getDeclaredField(AND_DEPENDENCIES);
			andDependencies.setAccessible(true);
			Map<Change, Set<Change>> dependen = new HashMap<>();
			dependen.put(first, new HashSet<Change>());
			dependen.get(first).add(second);
			
			andDependencies.set(deltaFact, dependen);
			List<Delta> deltas = new ArrayList<>();
			deltas.add(fail);
			assertEquals(1, deltas.size());
			Method method = deltaFactoryClass.getDeclaredMethod(PROCESS_CONSTRAINTS, List.class);
			method.setAccessible(true);
			method.invoke(deltaFact, deltas);
			assertTrue(deltas.isEmpty());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	/** Tests the private method processConstraints.
	 * Input: List contains one AND Constraint
	 * Return: unchange list of Deltas
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void processConstraintsSimpleANDListMatch() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {

			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			
			List<String> changeIDList = new ArrayList<>();
			changeIDList.add(first.getRef());
			changeIDList.add(second.getRef());

			DelElement delEle = new DelElement(new TargetMock(DELE_TARGET_NAME));
			List<DeltaElement> deltaElements = new ArrayList<>();
			deltaElements.add(delEle);
			Delta fail = new Delta(changeIDList, deltaElements);
			
			Field andDependencies = deltaFactoryClass.getDeclaredField(AND_DEPENDENCIES);
			andDependencies.setAccessible(true);
			Map<Change, Set<Change>> dependen = new HashMap<>();
			dependen.put(first, new HashSet<Change>());
			dependen.get(first).add(second);
			dependen.put(second, new HashSet<Change>());
			dependen.get(second).add(first);
			
			andDependencies.set(deltaFact, dependen);
			List<Delta> deltas = new ArrayList<>();
			deltas.add(fail);
			assertEquals(1, deltas.size());
			
			Method method = deltaFactoryClass.getDeclaredMethod(PROCESS_CONSTRAINTS, List.class);
			method.setAccessible(true);
			method.invoke(deltaFact, deltas);
			
			assertEquals(1, deltas.size());
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	/** Tests the private method processConstraints.
	 * Input: List contains one NOT Constraint
	 * Return: empty List since the one Delta is not in compliance with the Constraint.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void processConstraintsSimpleNOTListFail() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {

			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			
			List<String> changeIDList = new ArrayList<>();
			changeIDList.add(first.getRef());
			changeIDList.add(second.getRef());
			
			DelElement delEle = new DelElement(new TargetMock(DELE_TARGET_NAME));
			List<DeltaElement> deltaElements = new ArrayList<>();
			deltaElements.add(delEle);
			Delta fail = new Delta(changeIDList, deltaElements);
			
			Field notDependencies = deltaFactoryClass.getDeclaredField(NOT_DEPENDENCIES);
			notDependencies.setAccessible(true);
			Map<Change, Set<Change>> dependen = new HashMap<>();
			dependen.put(first, new HashSet<Change>());
			dependen.get(first).add(second);
			
			notDependencies.set(deltaFact, dependen);
			List<Delta> deltas = new ArrayList<>();
			deltas.add(fail);
			assertEquals(1, deltas.size());
			Method method = deltaFactoryClass.getDeclaredMethod(PROCESS_CONSTRAINTS, List.class);
			method.setAccessible(true);
			method.invoke(deltaFact, deltas);
			assertTrue(deltas.isEmpty());
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	/** Tests the private method processConstraints.
	 * Input: List contains one NOT Constraint
	 * Return: unchange list of Deltas
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void processConstraintsSimpleNOTListMatch() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {

			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			
			List<String> changeIDList = new ArrayList<>();
			changeIDList.add(first.getRef());
			
			DelElement delEle = new DelElement(new TargetMock(DELE_TARGET_NAME));
			List<DeltaElement> deltaElements = new ArrayList<>();
			deltaElements.add(delEle);
			Delta fail = new Delta(changeIDList, deltaElements);
			
			Field notDependencies = deltaFactoryClass.getDeclaredField(NOT_DEPENDENCIES);
			notDependencies.setAccessible(true);
			Map<Change, Set<Change>> dependen = new HashMap<>();
			dependen.put(first, new HashSet<Change>());
			dependen.get(first).add(second);
			
			notDependencies.set(deltaFact, dependen);
			List<Delta> deltas = new ArrayList<>();
			deltas.add(fail);
			assertEquals(1, deltas.size());
			Method method = deltaFactoryClass.getDeclaredMethod(PROCESS_CONSTRAINTS, List.class);
			method.setAccessible(true);
			method.invoke(deltaFact, deltas);
			assertEquals(1, deltas.size());
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	

	/** Tests the private method processConstraints.
	 * Input: List contains one REQ Constraint
	 * Return: empty List since the one Delta is not in compliance with the Constraint.
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void processConstraintsSimpleREQListFail() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {

			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			
			List<String> changeIDList = new ArrayList<>();
			changeIDList.add(first.getRef());
			
			DelElement delEle = new DelElement(new TargetMock(DELE_TARGET_NAME));
			List<DeltaElement> deltaElements = new ArrayList<>();
			deltaElements.add(delEle);
			Delta fail = new Delta(changeIDList, deltaElements);
			
			Field andDependencies = deltaFactoryClass.getDeclaredField("reqDependencies");
			andDependencies.setAccessible(true);
			Map<Change, Set<Change>> dependen = new HashMap<>();
			dependen.put(first, new HashSet<Change>());
			dependen.get(first).add(second);
			
			andDependencies.set(deltaFact, dependen);
			List<Delta> deltas = new ArrayList<>();
			deltas.add(fail);
			assertEquals(1, deltas.size());
			Method method = deltaFactoryClass.getDeclaredMethod(PROCESS_CONSTRAINTS, List.class);
			method.setAccessible(true);
			method.invoke(deltaFact, deltas);
			assertTrue(deltas.isEmpty());
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}	
	
	/** Tests the private method processConstraints.
	 * Input: List contains one REQ Constraint
	 * Return: unchange list of Deltas
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void processConstraintsSimpleREQListMatch() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {

			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			
			List<String> changeIDList = new ArrayList<>();
			changeIDList.add(first.getRef());
			changeIDList.add(second.getRef());
			
			DelElement delEle = new DelElement(new TargetMock(DELE_TARGET_NAME));
			List<DeltaElement> deltaElements = new ArrayList<>();
			deltaElements.add(delEle);
			Delta fail = new Delta(changeIDList, deltaElements);
			
			Field reqDependencies = deltaFactoryClass.getDeclaredField("reqDependencies");
			reqDependencies.setAccessible(true);
			Map<Change, Set<Change>> dependen = new HashMap<>();
			dependen.put(first, new HashSet<Change>());
			dependen.get(first).add(second);
			
			reqDependencies.set(deltaFact, dependen);
			List<Delta> deltas = new ArrayList<>();
			deltas.add(fail);
			assertEquals(1, deltas.size());
			Method method = deltaFactoryClass.getDeclaredMethod(PROCESS_CONSTRAINTS, List.class);
			method.setAccessible(true);
			method.invoke(deltaFact, deltas);
			assertEquals(1, deltas.size());
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
		
	
	
	/** Tests the private method processConstraints
	 * In total 12 Deltas. after running the method there will be to 2 left.
	 * Only Deltas which have an alternative from each Change will be left.
	 * 
	 * FirstChange=AND(SecondChange, ThirdChange)
	 */
	@SuppressWarnings("static-method")
	@Test 
	public final void processConstraintsAndList() {
		try {
			//-------- create test-data ----------//
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			List<Delta> deltas = createTestDataForConstraintsAndListTest(first, second, third);
			//-----------create test-data FINISHED-----//
			DeltaFactory deltaFact = new DeltaFactory();
			Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
			Field andDependencies = deltaFactoryClass.getDeclaredField(AND_DEPENDENCIES);
			andDependencies.setAccessible(true);
			Map<Change, Set<Change>> dependen = new HashMap<>();
			dependen.put(first, new HashSet<Change>());
			dependen.get(first).add(second);
			dependen.get(first).add(third);
			dependen.put(second, new HashSet<Change>());
			dependen.get(second).add(first);
			dependen.put(third, new HashSet<Change>());
			dependen.get(third).add(first);
			andDependencies.set(deltaFact, dependen);
			
			Method method = deltaFactoryClass.getDeclaredMethod(PROCESS_CONSTRAINTS, List.class);
			method.setAccessible(true);
			method.invoke(deltaFact, deltas);
			
			assertEquals(2, deltas.size());
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail("EXCEPTION");
		}
	}
	
	/** Creating data for ConstraintsAndList.
	 * @param first firstChange
	 * @param second secondChange
	 * @param third thirdChange
	 * @return a List of Deltas.
	 */
	private static List<Delta> createTestDataForConstraintsAndListTest(final Change first, final Change second, final Change third) {
		List<Change> changes = new ArrayList<>();
		EClass someMetaclass = null;
		try {
			someMetaclass = UMLHelper.getMetaClass("Stereotype");
		} catch (InvalidMetaclassException e) {
			fail(e.getMessage());
		}

		Alternative firstA = new Alternative();
		Alternative firstB = new Alternative();
		
		AddElement add = new AddElement(new TargetMock("firstAlterFC"), someMetaclass, null);
		firstA.addDeltaElement(add);
		
		AddElement addII = new AddElement(new TargetMock("SecondAlterFC"), someMetaclass, null);
		AddElement addIII = new AddElement(new TargetMock("SecondAlterFC2"), someMetaclass, null);
		firstB.addDeltaElement(addII);
		firstB.addDeltaElement(addIII);
		
		first.addAlternative(firstA);
		first.addAlternative(firstB);

		Alternative secondA = new Alternative();
		SubstElement subst = new SubstElement(new TargetMock("firstAlterSC"), new ArrayList<AddElement>());
		secondA.addDeltaElement(subst);			
		second.addAlternative(secondA);
		
		Alternative thirdA = new Alternative();
		DelElement del = new DelElement(new TargetMock("firstAlterTC"));
		assertEquals("firstAlterTC", ((TargetMock) del.getTarget()).getName());
		thirdA.addDeltaElement(del);
		third.addAlternative(thirdA);
		
		changes.add(first);
		changes.add(second);
		changes.add(third);
		
		List<DeltaElement> content = new ArrayList<>();
		List<Delta> deltas = new ArrayList<>();
		List<String> changeIDs = new ArrayList<>();
		
		content.addAll(firstA.getDeltaElements());
		content.addAll(secondA.getDeltaElements());
		content.addAll(thirdA.getDeltaElements());
		changeIDs.add(FIRST_CHANGE);
		changeIDs.add(SECOND_CHANGE);
		changeIDs.add(THIRD_CHANGE);
		
		Delta d1 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(firstB.getDeltaElements());
		content.addAll(secondA.getDeltaElements());	
		content.addAll(thirdA.getDeltaElements());		
		changeIDs.add(FIRST_CHANGE);
		changeIDs.add(SECOND_CHANGE);
		changeIDs.add(THIRD_CHANGE);
		
		Delta d2 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(secondA.getDeltaElements());
		content.addAll(thirdA.getDeltaElements());
		changeIDs.add(SECOND_CHANGE);
		changeIDs.add(THIRD_CHANGE);
		
		Delta d3 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(firstA.getDeltaElements());
		content.addAll(thirdA.getDeltaElements());	
		changeIDs.add(FIRST_CHANGE);
		changeIDs.add(THIRD_CHANGE);
		Delta d4 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(firstB.getDeltaElements());
		content.addAll(thirdA.getDeltaElements());
		changeIDs.add(FIRST_CHANGE);
		changeIDs.add(THIRD_CHANGE);
		
		Delta d5 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();

		content.addAll(thirdA.getDeltaElements());
		changeIDs.add(THIRD_CHANGE);
		
		Delta d6 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(firstA.getDeltaElements());
		content.addAll(secondA.getDeltaElements());		
		changeIDs.add(FIRST_CHANGE);
		changeIDs.add(SECOND_CHANGE);
		Delta d7 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(firstB.getDeltaElements());
		content.addAll(secondA.getDeltaElements());		
		changeIDs.add(FIRST_CHANGE);
		changeIDs.add(SECOND_CHANGE);
		Delta d8 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(secondA.getDeltaElements());
		changeIDs.add(SECOND_CHANGE);
		Delta d9 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(firstA.getDeltaElements());	
		changeIDs.add(FIRST_CHANGE);
		Delta d10 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		content.addAll(firstB.getDeltaElements());
		changeIDs.add(FIRST_CHANGE);
		Delta d11 = new Delta(changeIDs, content);
		content.clear();
		changeIDs.clear();
		
		deltas.add(d1);
		deltas.add(d2);
		deltas.add(d3);
		deltas.add(d4);
		deltas.add(d5);
		deltas.add(d6);
		deltas.add(d7);
		deltas.add(d8);
		deltas.add(d9);
		deltas.add(d10);
		deltas.add(d11);	
		return deltas;
	}
	
	
	/** Test the private method parseConstraints.
	 *  Empty input.
	 */
	@SuppressWarnings({ "unchecked", "static-method" })
	@Test
	public final void parseConstraintsEmptyInputTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			Method method = deltaFactoryClass.getDeclaredMethod(PARSE_CONSTRAINTS);
			method.setAccessible(true);
			List<Change> changes = new ArrayList<>();
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			Change fourth = new Change(FOURTH_CHANGE);
			changes.add(first);
			changes.add(second);
			changes.add(third);
			changes.add(fourth);
			
			Method init = deltaFactoryClass.getDeclaredMethod("init", List.class);
			init.setAccessible(true);
			init.invoke(deltaFact, changes);			
			
			method.invoke(deltaFact);
			Field andDependencies = deltaFactoryClass.getDeclaredField(AND_DEPENDENCIES);
			Field notDependencies = deltaFactoryClass.getDeclaredField(NOT_DEPENDENCIES);
			Field reqDependencies = deltaFactoryClass.getDeclaredField(FIELD_REQ_DEPENDENCIES);
			
			andDependencies.setAccessible(true);
			notDependencies.setAccessible(true);
			reqDependencies.setAccessible(true);
			assertEquals(0, ((Map<Change, Set<Change>>) andDependencies.get(deltaFact)).size());
			assertEquals(0, ((Map<Change, Set<Change>>) notDependencies.get(deltaFact)).size());
			assertEquals(0, ((Map<Change, Set<Change>>) reqDependencies.get(deltaFact)).size());
		 	
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
	}
	
	/** Test the private method parseConstraints.
	 * First=AND(Second,Third)
	 * Third=AND(First,Second,Fourth)
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void parseConstraintsAndInputTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		
		try {
			Method method = deltaFactoryClass.getDeclaredMethod(PARSE_CONSTRAINTS);
			method.setAccessible(true);
			List<Change> changes = new ArrayList<>();
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			Change fourth = new Change(FOURTH_CHANGE);	
			
			ChangeConstraint firstSecondAnd = new ChangeConstraint(ConstraintType.AND, second, first);
			ChangeConstraint firstThirdAnd = new ChangeConstraint(ConstraintType.AND, third, first);
			first.addConstraint(firstSecondAnd);
			first.addConstraint(firstThirdAnd);

			ChangeConstraint thirdFirstAND = new ChangeConstraint(ConstraintType.AND, first, third);
			ChangeConstraint thirdSecondAND = new ChangeConstraint(ConstraintType.AND, second, third);
			ChangeConstraint thirdFourthAND = new ChangeConstraint(ConstraintType.AND, fourth, third);
			third.addConstraint(thirdFirstAND);
			third.addConstraint(thirdSecondAND);
			third.addConstraint(thirdFourthAND);
			
			
			changes.add(first);
			changes.add(second);
			changes.add(third);
			changes.add(fourth);
			
			Method init = deltaFactoryClass.getDeclaredMethod("init", List.class);
			init.setAccessible(true);
			init.invoke(deltaFact, changes);		
			
			method.invoke(deltaFact);
			Field andDependencies = deltaFactoryClass.getDeclaredField(AND_DEPENDENCIES);
			andDependencies.setAccessible(true);
			@SuppressWarnings("unchecked")
			Map<Change, Set<Change>> andDep = (Map<Change, Set<Change>>) andDependencies.get(deltaFact);
			
			assertEquals(4, andDep.size());
			assertTrue(andDep.containsKey(first));
			assertEquals(2, andDep.get(first).size());
			assertTrue(andDep.get(first).contains(second));
			assertTrue(andDep.get(first).contains(third));
			
			assertTrue(andDep.containsKey(second));
			assertEquals(2, andDep.get(second).size());
			assertTrue(andDep.get(second).contains(first));
			assertTrue(andDep.get(second).contains(third));
			assertFalse(andDep.get(second).contains(second));
	
			assertTrue(andDep.containsKey(third));
			assertEquals(3, andDep.get(third).size());
			assertTrue(andDep.get(third).contains(first));
			assertTrue(andDep.get(third).contains(second));
			assertTrue(andDep.get(third).contains(fourth));

			assertTrue(andDep.containsKey(fourth));
			assertEquals(1, andDep.get(fourth).size());
			assertTrue(andDep.get(fourth).contains(third));
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}

	
	/** Test the private method parseConstraints.
	 * First=NOT(Second,Third)
	 */
	@SuppressWarnings("static-method")
	@Test
	public final void parseConstraintsNotInputTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			Method method = deltaFactoryClass.getDeclaredMethod(PARSE_CONSTRAINTS);
			method.setAccessible(true);
			List<Change> changes = new ArrayList<>();
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			
			ChangeConstraint firstSecondAnd = new ChangeConstraint(ConstraintType.NOT, second, first);
			ChangeConstraint firstThirdAnd = new ChangeConstraint(ConstraintType.NOT, third, first);
			first.addConstraint(firstSecondAnd);
			first.addConstraint(firstThirdAnd);


			changes.add(first);
			changes.add(second);
			changes.add(third);
			
			Method init = deltaFactoryClass.getDeclaredMethod("init", List.class);
			init.setAccessible(true);
			init.invoke(deltaFact, changes);
				
			Field changesField = deltaFactoryClass.getDeclaredField(FIELD_CHANGES);
			changesField.setAccessible(true);
			changesField.set(deltaFact, changes);
			
			
			method.invoke(deltaFact);
			
			Field notDependencies = deltaFactoryClass.getDeclaredField(NOT_DEPENDENCIES);
			notDependencies.setAccessible(true);
			@SuppressWarnings("unchecked")
			Map<Change, Set<Change>> notDep = (Map<Change, Set<Change>>) notDependencies.get(deltaFact);
			
			
			assertEquals(1, notDep.size());
			assertTrue(notDep.containsKey(first));
			assertEquals(2, notDep.get(first).size());
			assertTrue(notDep.get(first).contains(second));
			assertTrue(notDep.get(first).contains(third));
			
			assertFalse(notDep.containsKey(second));
			assertFalse(notDep.containsKey(third));
				
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	/** Test the private method parseConstraints.
	 * Second=REQ(First)
	 * Third=REQ(Second)
	 */
	@Test
	@Ignore
	public final void parseConstraintsReqInputTest() {
		DeltaFactory deltaFact = new DeltaFactory();

		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			Method method = deltaFactoryClass.getDeclaredMethod(PARSE_CONSTRAINTS);
			method.setAccessible(true);
			List<Change> changes = new ArrayList<>();
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			
			ChangeConstraint secondReqFirst = new ChangeConstraint(ConstraintType.REQ, first, second);
			ChangeConstraint thirdReqSecond = new ChangeConstraint(ConstraintType.REQ, second, third);
			
			second.addConstraint(secondReqFirst);
			third.addConstraint(thirdReqSecond);


			changes.add(first);
			changes.add(second);
			changes.add(third);
			
			DeltaFactory.getDeltas(changes);
			
			method.invoke(deltaFact);
			
			Field reqDependencies = deltaFactoryClass.getDeclaredField(FIELD_REQ_DEPENDENCIES);
			reqDependencies.setAccessible(true);
			@SuppressWarnings("unchecked")
			Map<Change, Set<Change>> reqDep = (Map<Change, Set<Change>>) reqDependencies.get(deltaFact);

			assertEquals(2, reqDep.size());
			
			assertFalse(reqDep.containsKey(first));
			
			assertTrue(reqDep.containsKey(second));
			assertEquals(1, reqDep.get(second).size());
			assertTrue(reqDep.get(second).contains(first));

			assertTrue(reqDep.containsKey(third));
			assertEquals(1, reqDep.get(third).size());
			assertFalse(reqDep.containsKey(first));
				
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}

	
	/** Test the private method sortList.
	 * FirstChange REQ ThirdChange
	 * FourthChange REQ SecondChange
	 * 
	 * So by all means the ThirdChange has to be used before the FirstChange
	 * and the SecondChange has to be used before the FourthChange.
	 */	
	@SuppressWarnings("static-method")
	@Test
	public final void sortChangesTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			Method method = deltaFactoryClass.getDeclaredMethod("sortChanges");
			method.setAccessible(true);
			List<Change> changes = new ArrayList<>();
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			Change fourth = new Change(FOURTH_CHANGE);
			
			ChangeConstraint firstReqThird = new ChangeConstraint(ConstraintType.REQ, third, first);
			ChangeConstraint fourthReqSecond = new ChangeConstraint(ConstraintType.REQ, second, fourth);
			
			second.addConstraint(firstReqThird);
			third.addConstraint(fourthReqSecond);
			
			Map<Change, Set<Change>> reqDependencies = new HashMap<>();
			reqDependencies.put(first, new HashSet<Change>());
			reqDependencies.get(first).add(third);
			
			reqDependencies.put(fourth, new HashSet<Change>());
			reqDependencies.get(fourth).add(second);
			
			Field reqDependencies2 = deltaFactoryClass.getDeclaredField(FIELD_REQ_DEPENDENCIES);
			reqDependencies2.setAccessible(true);
			reqDependencies2.set(deltaFact, reqDependencies);

			changes.add(first);
			changes.add(second);
			changes.add(third);
			changes.add(fourth);
			
			Field changesField = deltaFactoryClass.getDeclaredField(FIELD_CHANGES);
			changesField.setAccessible(true);
			changesField.set(deltaFact, changes);
			
			
			method.invoke(deltaFact);
			
			boolean thirdBeforeFirst = false;
			boolean secondBeforeFourth = false;

			for (Change ch : changes) {
				if (!thirdBeforeFirst && ch.equals(first)) {
					fail("FirstChange came before ThirdChange");
				} else if (ch.equals(second)) {
					secondBeforeFourth = true;
				} else if (ch.equals(third)) {
					thirdBeforeFirst = true;
				} else if (!secondBeforeFourth && ch.equals(fourth)) {
					fail("FourthChange came before SecondChange");
				}
			}
			assertTrue(thirdBeforeFirst);
			assertTrue(secondBeforeFourth);
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	
	/** Test the private method sortList.
	 * FirstChange REQ ThirdChange
	 * ThirdChange REQ SecondChange
	 * ThirdChange REQ FourthChange
	 * FourthChange REQ SecondChange
	 * 
	 * The Expected order of used Changes is:
	 * SecondChange - FourthChange - ThirdChange - FirstChange
	 */	
	@SuppressWarnings("static-method")
	@Test
	public final void sortChangesChainTest() {
		DeltaFactory deltaFact = new DeltaFactory();
		Class<? extends DeltaFactory> deltaFactoryClass = deltaFact.getClass();
		try {
			Method method = deltaFactoryClass.getDeclaredMethod("sortChanges");
			method.setAccessible(true);
			List<Change> changes = new ArrayList<>();
			Change first = new Change(FIRST_CHANGE);
			Change second = new Change(SECOND_CHANGE);
			Change third = new Change(THIRD_CHANGE);
			Change fourth = new Change(FOURTH_CHANGE);
			
			Map<Change, Set<Change>> reqDependencies = new HashMap<>();
			reqDependencies.put(first, new HashSet<Change>());
			reqDependencies.get(first).add(third);
			
			reqDependencies.put(third, new HashSet<Change>());
			reqDependencies.get(third).add(second);
			reqDependencies.get(third).add(fourth);
			
			reqDependencies.put(fourth, new HashSet<Change>());
			reqDependencies.get(fourth).add(second);
			
			Field reqDependencies2 = deltaFactoryClass.getDeclaredField(FIELD_REQ_DEPENDENCIES);
			reqDependencies2.setAccessible(true);
			reqDependencies2.set(deltaFact, reqDependencies);

			
			changes.add(first);
			changes.add(second);
			changes.add(third);
			changes.add(fourth);
			
			Field changesField = deltaFactoryClass.getDeclaredField(FIELD_CHANGES);
			changesField.setAccessible(true);
			changesField.set(deltaFact, changes);
			
			method.invoke(deltaFact);

			assertEquals(4, changes.size());
			assertEquals(second, changes.get(0));
			assertEquals(fourth, changes.get(1));
			assertEquals(third, changes.get(2));
			assertEquals(first, changes.get(3));
			
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	/** Test if the getDelta() method create the expected List of Delta when
	 * no constraint is set. Input is a List with a single Change, "first".
	 * "first" has two Alternatives.
	 * In Total there are 3 different Deltas possible, the output is expected to have 2 Deltas because
	 * the Delta which uses none of the Changes shall be ignored.
	 */
	@Test
	@Ignore
	public final void getDeltaSimpleTest() {
		//------TestDataCreation----------------------
		EClass someMetaclass = null;
		try {
			someMetaclass = UMLHelper.getMetaClass("Stereotype");
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.INFO, "", e);
			fail(e.getMessage());
		}
		EObject firstADeltaEleTarget = new TargetMock("FirstChange, Altern. A");
		EObject firstBDeltaEleTarget = new TargetMock("FirstChange, Altern. B");
		AddElement firstADeltaEle = new AddElement(firstADeltaEleTarget, someMetaclass, null);
		AddElement firstBDeltaEle = new AddElement(firstBDeltaEleTarget, someMetaclass, null);

		List<Change> changes = new ArrayList<>();
		Change first = new Change(FIRST_CHANGE);
		
		Alternative firstA = new Alternative();
		Alternative firstB = new Alternative();
		first.addAlternative(firstA);
		first.addAlternative(firstB);

		firstA.addDeltaElement(firstADeltaEle);
		firstB.addDeltaElement(firstBDeltaEle);
		
		changes.add(first);
		//------TestDataCreation Finished-------------
		
		List<Delta> deltas = DeltaFactory.getDeltas(changes);
		assertEquals(2, deltas.size());

		assertEquals(1, deltas.get(0).getNumberOfUsedChanges());
		assertTrue(deltas.get(0).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_ONE));
		

		assertEquals(1, deltas.get(1).getNumberOfUsedChanges());
		assertTrue(deltas.get(1).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_TWO));
	}
	
	/** Test if the getDelta() method create the expected List of Delta when
	 * no constraint is set. Input is a List with two Changes, Change "first" and Change "second".
	 * "first" has two Alternatives and "second" has three Alternatives. 
	 * In Total there are 12 different Deltas possible, the output is expected to have 11 Deltas because
	 * the Delta which uses none of the Changes shall be ignored.
	 */
	@Test
	@Ignore
	public final void getDeltaTest() {
		//------TestDataCreation----------------------
		
		//creating MetaClass for DeltaElements
		EClass someMetaclass = null;
		try {
			someMetaclass = UMLHelper.getMetaClass("Stereotype");
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.INFO, "", e);
			fail(e.getMessage());
		}
		//creating Targets to differ DeltaElements
		EObject firstADeltaEleTarget = new TargetMock("FirstChange, Altern. A");
		EObject firstBDeltaEleTarget = new TargetMock("FirstChange, Altern. B");
		EObject secondADeltaEleTarget = new TargetMock("SecondChange, Altern. A");
		EObject secondBDeltaEleTarget = new TargetMock("SecondChange, Altern. B");
		EObject secondCDeltaEleTarget = new TargetMock("SecondChange, Altern. C");
		//creating DeltaElements 
		AddElement firstADeltaEle = new AddElement(firstADeltaEleTarget, someMetaclass, null);
		AddElement firstBDeltaEle = new AddElement(firstBDeltaEleTarget, someMetaclass, null);		

		AddElement secondADeltaEle = new AddElement(secondADeltaEleTarget, someMetaclass, null);
		AddElement secondBDeltaEle = new AddElement(secondBDeltaEleTarget, someMetaclass, null);
		AddElement secondCDeltaEle = new AddElement(secondCDeltaEleTarget, someMetaclass, null);
		
		//creating Changes
		List<Change> changes = new ArrayList<>();
		Change first = new Change(FIRST_CHANGE);		
		Change second = new Change(SECOND_CHANGE);
		
		//creating Alternatives
		Alternative firstA = new Alternative();
		Alternative firstB = new Alternative();		
		first.addAlternative(firstA);
		first.addAlternative(firstB);

		firstA.addDeltaElement(firstADeltaEle);
		firstB.addDeltaElement(firstBDeltaEle);

		Alternative secondA = new Alternative();
		Alternative secondB = new Alternative();
		Alternative secondC = new Alternative();
		second.addAlternative(secondA);
		second.addAlternative(secondB);
		second.addAlternative(secondC);

		secondA.addDeltaElement(secondADeltaEle);
		secondB.addDeltaElement(secondBDeltaEle);
		secondC.addDeltaElement(secondCDeltaEle);
		
		changes.add(first);
		changes.add(second);
		//------TestDataCreation Finished-------------

		
		List<Delta> deltas = DeltaFactory.getDeltas(changes);
		
		
		assertEquals(11, deltas.size());

		assertEquals(2, deltas.get(0).getNumberOfUsedChanges());
		assertTrue(deltas.get(0).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_ONE));
		assertTrue(deltas.get(0).getChangeIDs().contains(SECOND_CHANGE + ALTERNATIVE_ONE));

		assertEquals(2, deltas.get(1).getNumberOfUsedChanges());
		assertTrue(deltas.get(1).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_TWO));
		assertTrue(deltas.get(1).getChangeIDs().contains(SECOND_CHANGE + ALTERNATIVE_ONE));

		assertEquals(1, deltas.get(2).getNumberOfUsedChanges());
		assertTrue(deltas.get(2).getChangeIDs().contains(SECOND_CHANGE + ALTERNATIVE_ONE));

		assertEquals(2, deltas.get(3).getNumberOfUsedChanges());
		assertTrue(deltas.get(3).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_ONE));
		assertTrue(deltas.get(3).getChangeIDs().contains(SECOND_CHANGE + ALTERNATIVE_TWO));

		assertEquals(2, deltas.get(4).getNumberOfUsedChanges());
		assertTrue(deltas.get(4).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_TWO));
		assertTrue(deltas.get(4).getChangeIDs().contains(SECOND_CHANGE + ALTERNATIVE_TWO));

		assertEquals(1, deltas.get(5).getNumberOfUsedChanges());
		assertTrue(deltas.get(5).getChangeIDs().contains(SECOND_CHANGE + ALTERNATIVE_TWO));

		assertEquals(2, deltas.get(6).getNumberOfUsedChanges());
		assertTrue(deltas.get(6).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_ONE));
		assertTrue(deltas.get(6).getChangeIDs().contains(SECOND_CHANGE + " (Alt.3)"));

		assertEquals(2, deltas.get(7).getNumberOfUsedChanges());
		assertTrue(deltas.get(7).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_TWO));
		assertTrue(deltas.get(7).getChangeIDs().contains(SECOND_CHANGE + " (Alt.3)"));

		assertEquals(1, deltas.get(8).getNumberOfUsedChanges());
		assertTrue(deltas.get(8).getChangeIDs().contains(SECOND_CHANGE + " (Alt.3)"));

		assertEquals(1, deltas.get(9).getNumberOfUsedChanges());
		assertTrue(deltas.get(9).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_ONE));

		assertEquals(1, deltas.get(10).getNumberOfUsedChanges());
		assertTrue(deltas.get(10).getChangeIDs().contains(FIRST_CHANGE + ALTERNATIVE_TWO));
	}
	

	
	/** Simple Mock to differ chosen Alternatives.
	 * 
	 * @author B. Berghoff
	 *
	 */
	private static class TargetMock implements EObject {
		/** Name of the Target. 
		 */
		private String name;
		
		/** public constructor.
		 * 
		 * @param name Name
		 */
		public TargetMock(final String name) { 
			this.name = name; 
		}
		/** Get the Name of the Target.
		 * @return String name of the Target.
		 */
		public String getName() {
			return this.name;
		}
		
		@Override
		public EList<Adapter> eAdapters() {
			return null;
		}

		@Override
		public boolean eDeliver() {
			return false;
		}

		@Override
		public void eSetDeliver(final boolean deliver) {
		}

		@Override
		public void eNotify(final Notification notification) {
		}

		@Override
		public EClass eClass() {
			return null;
		}

		@Override
		public Resource eResource() {
			return null;
		}

		@Override
		public EObject eContainer() {
			return null;
		}

		@Override
		public EStructuralFeature eContainingFeature() {
			return null;
		}

		@Override
		public EReference eContainmentFeature() {
			return null;
		}

		@Override
		public EList<EObject> eContents() {
			return null;
		}

		@Override
		public TreeIterator<EObject> eAllContents() {	
			return null;
		}

		@Override
		public boolean eIsProxy() {
			
			return false;
		}

		@Override
		public EList<EObject> eCrossReferences() {
			
			return null;
		}

		@Override
		public Object eGet(final EStructuralFeature feature) {
			
			return null;
		}

		@Override
		public Object eGet(final EStructuralFeature feature, final boolean resolve) {
			return null;
		}

		@Override
		public void eSet(final EStructuralFeature feature, final Object newValue) {
		}

		@Override
		public boolean eIsSet(final EStructuralFeature feature) {
			return false;
		}

		@Override
		public void eUnset(final EStructuralFeature feature) {
		}

		@Override
		public Object eInvoke(final EOperation operation, final EList<?> arguments)
				throws InvocationTargetException {
			return null;
		} 
	}
}
