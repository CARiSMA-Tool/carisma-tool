package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.UMLFactory;
import org.eclipse.uml2.uml.UMLPackage;
import org.junit.After;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;
import carisma.tests.modelutils.uml.TestHelper;

/**
 * 
 */
public class ComplexChangesParserTest {
	
	/**
	 * Path to the test models.
	 */
	private String testmodeldir = "resources/models/complexChanges";
			
	/**
	 * Constant variable for Attribute owner of an EditElement.
	 */
	private static final String OWNER = "owner";
	
	/** 
	 * Constant field for "targetPkg" as a Name of a NamedElement.
	 */
	private static final String TARGET_PKG = "targetPkg";
	
	/**
	 * Constant variable for Attribute name of an AddElement.
	 */
	private static final String NAME = "name";
	
	/**
	 * Test model.
	 */
	private Model model;
	
	@Test
	public void testAddSingleComplexClass() {
		model = TestHelper.loadModel(testmodeldir, "addSingleComplexClass.uml");
		assertNotNull(model.getAppliedProfile(UMLchange.DESCRIPTOR.getProfileName()));
		List<StereotypeApplication> applications = UMLchangeUtil.getStereotypeApplications(model);
		assertNotNull(applications);
		assertEquals(1, applications.size());
		UMLchangeParser parser = new UMLchangeParser(model);
		assertNotNull(parser);
		List<Change> changes = parser.generateDeltaDescriptions();
		assertNotNull(changes);
		assertEquals(1, changes.size());
		Change complexChange = changes.get(0);
		assertNotNull(complexChange);
		assertEquals(1, complexChange.getAlternatives().size());
		assertEquals("addThis", complexChange.getRef());
		Alternative complexAlternative = complexChange.getAlternatives().get(0);
		assertNotNull(complexAlternative);
		List<DeltaElement> complexElements = complexAlternative.getDeltaElements();
		assertNotNull(complexElements);
		assertEquals(1, complexElements.size());
		DeltaElement de = complexElements.get(0);
		assertEquals(EditElement.class, de.getClass());
		EditElement edit = (EditElement) de;
		assertEquals(1, edit.getValues().size());
		assertTrue(edit.getValues().containsKey(OWNER));
		assertEquals(model.getMember(TARGET_PKG), edit.getValues().get(OWNER));
	}
	
	@Test
	public void testAddMultipleClasses() {
		model = TestHelper.loadModel(testmodeldir, "addMultipleClasses.uml");
		assertNotNull(model.getAppliedProfile(UMLchange.DESCRIPTOR.getProfileName()));
		List<StereotypeApplication> applications = UMLchangeUtil.getStereotypeApplications(model);
		assertEquals(1, applications.size());
		UMLchangeParser parser = new UMLchangeParser(model);
		List<Change> changes = parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		Change complexChange = changes.get(0);
		assertNotNull(complexChange);
		Alternative complexAlternative = complexChange.getAlternatives().get(0);
		assertNotNull(complexAlternative);
		List<DeltaElement> complexElements = changes.get(0).getAlternatives().get(0).getDeltaElements();
		assertEquals(3, complexElements.size());
		for (DeltaElement de : complexElements) {
			assertEquals(EditElement.class, de.getClass());
			EditElement edit = (EditElement) de;
			assertEquals(1, edit.getValues().size());
			assertTrue(edit.getValues().containsKey(OWNER));
			assertEquals(model.getMember(TARGET_PKG), edit.getValues().get(OWNER));
		}
	}
	
	@Test
	public void testAddMultipleElementsWithConnections() {
		model = TestHelper.loadModel(testmodeldir, "addMultipleWithConnections.uml");
		assertNotNull(model.getAppliedProfile(UMLchange.DESCRIPTOR.getProfileName()));
		List<StereotypeApplication> applications = UMLchangeUtil.getStereotypeApplications(model);
		assertEquals(1, applications.size());
		UMLchangeParser parser = new UMLchangeParser(model);
		List<Change> changes = parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		List<DeltaElement> complexElements = changes.get(0).getAlternatives().get(0).getDeltaElements();
		assertEquals(2, complexElements.size());
		for (DeltaElement de : complexElements) {
			assertEquals(EditElement.class, de.getClass());
			EditElement edit = (EditElement) de;
			assertEquals(1, edit.getValues().size());
			assertTrue(edit.getValues().containsKey(OWNER));
			assertEquals(model.getMember(TARGET_PKG), edit.getValues().get(OWNER));
		}
	}
	
	@Test
	public void testAddToOldClass() {
		model = TestHelper.loadModel(testmodeldir, "addToOldClass.uml");
		try {
			assertNotNull(model.getAppliedProfile(UMLchange.DESCRIPTOR.getProfileName()));
			List<StereotypeApplication> applications = UMLchangeUtil.getStereotypeApplications(model);
			assertEquals(4, applications.size());
			UMLchangeParser parser = new UMLchangeParser(model);
			List<Change> changes = parser.generateDeltaDescriptions();
			assertEquals(1, changes.size());
			List<DeltaElement> complexElements = changes.get(0).getAlternatives().get(0).getDeltaElements();
			assertEquals(1, complexElements.size());
			EditElement edit = (EditElement) complexElements.get(0);
			assertEquals(1, edit.getValues().size());
			assertTrue(edit.getValues().containsKey(OWNER));
			assertEquals(UMLHelper.getElementByName(model, "targetPkg::OldClass"), edit.getValues().get(OWNER));
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
	}

	@Test
	public void testAddExtensions() {
		model = TestHelper.loadModel(testmodeldir, "addExtensions.uml");
		try {
			assertNotNull(model.getAppliedProfile(UMLchange.DESCRIPTOR.getProfileName()));
			assertNotNull(model.getAppliedProfile("UMLsec"));
			List<StereotypeApplication> applications = UMLchangeUtil.getStereotypeApplications(model);
			assertEquals(2, applications.size());
			UMLchangeParser parser = new UMLchangeParser(model);
			List<Change> changes = parser.generateDeltaDescriptions();
			assertEquals(2, changes.size());
			List<DeltaElement> addThisElements = changes.get(0).getAlternatives().get(0).getDeltaElements();
			assertEquals(1, addThisElements.size());
			AddElement add = (AddElement) addThisElements.get(0);
			assertEquals(UMLPackage.eINSTANCE.getStereotype(), add.getMetaClass());
			assertEquals(1, add.getValues().size());
			assertTrue(add.getValues().containsKey(NAME));
			Element oldClass = UMLHelper.getElementByName(model, "OldClass");
			assertNotNull(oldClass);
			assertEquals(oldClass, add.getTarget());
			assertEquals("UMLsec::critical", add.getValues().get(NAME));
			List<DeltaElement> addThatElements = changes.get(1).getAlternatives().get(0).getDeltaElements();
			assertEquals(2, addThatElements.size());
			add = (AddElement) addThatElements.get(0);
			assertEquals(UMLPackage.eINSTANCE.getProperty(), add.getMetaClass());
			assertEquals(2, add.getValues().size());
			assertTrue(add.getValues().containsKey(NAME));
			oldClass = UMLHelper.getElementByName(model, "OldExtendedClass");
			assertNotNull(oldClass);
			Stereotype critical = oldClass.getAppliedStereotype("UMLsec::critical");
			StereotypeApplication oldCriticalApp = new StereotypeApplication(critical, oldClass);
			assertEquals(oldCriticalApp, add.getTarget());
			assertTrue(add.getValues().containsKey(NAME));
			assertEquals("secrecy", add.getValues().get(NAME));
			assertTrue(add.getValues().containsKey("value"));
			List<String> valueList = new ArrayList<String>();
			valueList.add("bla");
			assertEquals(valueList, add.getValues().get("value"));
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
	}

	@Test
	public void testAddNewConnectionToOld() {
		model = TestHelper.loadModel(testmodeldir, "addNewConnectionToOld.uml");
		try {
			assertNotNull(model.getAppliedProfile(UMLchange.DESCRIPTOR.getProfileName()));
			List<StereotypeApplication> applications = UMLchangeUtil.getStereotypeApplications(model);
			assertEquals(4, applications.size());
			UMLchangeParser parser = new UMLchangeParser(model);
			List<Change> changes = parser.generateDeltaDescriptions();
			assertEquals(1, changes.size());
			List<DeltaElement> complexElements = changes.get(0).getAlternatives().get(0).getDeltaElements();
			assertEquals(3, complexElements.size());
			for (DeltaElement de : complexElements) {
				EditElement edit = (EditElement) de;
				if (edit.getTarget().equals(UMLHelper.getElementByName(model, "testAss1"))) {
					assertEquals(2, edit.getValues().size());
					assertTrue(edit.getValues().containsKey("end1"));
					assertTrue(edit.getValues().containsKey("end2"));
					String end1Value = (String) edit.getValues().get("end1"); 
					String end2Value = (String) edit.getValues().get("end2");
					assertEquals("add::targetPkg::addThis::OldClass;;add::targetPkg::OldClass", end1Value);
					assertEquals("add::targetPkg::addThis::SecondOldClass;;add::targetPkg::SecondOldClass", end2Value);
				} else if (edit.getTarget().equals(UMLHelper.getElementByName(model, "testDep1"))) {
					assertTrue(edit.getValues().containsKey("client"));
					assertEquals(UMLHelper.getElementByName(model, "targetPkg::ThirdClass"), edit.getValues().get("client"));
				} else if (edit.getTarget().equals(UMLHelper.getElementByName(model, "NewClass"))) {
					assertTrue(edit.getValues().containsKey(OWNER));
					assertEquals(UMLHelper.getElementByName(model, TARGET_PKG), edit.getValues().get(OWNER));
				} else {
					assertTrue(false);
				}
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
	}
	
	@Test
	public void testWhatAreNamedElements() {
		// ref: refID
		// ext: refID=critical.high
		// new: Property(name=high,value=newValue)
		for (EClassifier ec : UMLPackage.eINSTANCE.getEClassifiers()) {
			if (ec instanceof EClass) {
				EClass eclss = (EClass) ec;
				if (!eclss.isAbstract()) {
					EObject createdEClass = UMLFactory.eINSTANCE.create(eclss);
					if (createdEClass instanceof Element && !(createdEClass instanceof NamedElement)) {
						Logger.log(LogLevel.INFO, createdEClass.toString());
					}
				}
			}
		}
	}
	
	/**	
	 * unloads the model.
	 */
	@After
	public final void unload() {
		if (model != null) {
			TestHelper.unloadModel(model);
			model = null;
		}
	}
}
