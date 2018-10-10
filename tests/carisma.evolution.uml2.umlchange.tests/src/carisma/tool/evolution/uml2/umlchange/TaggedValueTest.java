package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;
import carisma.tests.modelutils.uml.TestHelper;

public class TaggedValueTest {
	
	/**
	 * Path to the test models.
	 */
	private String testmodeldir = "resources/models/other";
		
	/**
	 * static name of the only model used in this test class.
	 */
	private static final String MODEL_NAME = "TaggedValueTest.uml";
	
	/**
	 * static name for Class1 in model MODEL_NAME.
	 */
	private static final String CLASS_ONE = "Class1";
	
	/**
	 * static name for Class2 in model MODEL_NAME.
	 */
	private static final String CLASS_TWO = "Class2";
	
	/**
	 * Test model.
	 */
	private Model model = null;
			
	@Test
	public final void testEmptyTagsAlreadyApplied() {
		this.model = TestHelper.loadModel(this.testmodeldir, MODEL_NAME);
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		try {
			Class class1 = UMLHelper.getElementOfNameAndType(this.model, CLASS_ONE, Class.class);
			assertNotNull(class1);
			Class class2 = UMLHelper.getElementOfNameAndType(this.model, CLASS_TWO, Class.class);
			assertNotNull(class2);
			StereotypeApplication addApp2 = UMLchangeUtil.getStereotypeApplication(UMLchange.ADD, class2);
			assertNotNull(addApp2);
			for (TaggedValue tv : addApp2.getTaggedValues()) {
				Object value = addApp2.getExtendedElement().getValue(addApp2.getAppliedStereotype(), tv.getName());
				assertNotNull(value);
			}
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "At least one expected elemtent hasn't been found.", e);
			fail(e.getMessage());
		}
	}
	
	@Test
	public final void testEmptyTagsNewApplication() {
		this.model = TestHelper.loadModel(this.testmodeldir, MODEL_NAME);
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		try {
			Class class1 = UMLHelper.getElementOfNameAndType(this.model, CLASS_ONE, Class.class);
			assertNotNull(class1);
			Class class2 = UMLHelper.getElementOfNameAndType(this.model, CLASS_TWO, Class.class);
			assertNotNull(class2);
			StereotypeApplication addApp1 = UMLHelper.applyStereotype(class1, "UMLchange::add");
			assertNotNull(addApp1);
			for (TaggedValue tv : addApp1.getTaggedValues()) {
				assertNotNull(tv.getValue());
			}
		} catch (ModelElementNotFoundException e) {
			
			fail("At least one expected elemtent hasn't been found. \n " + e.getMessage());
		}		
	}
	
	@Test
	public final void testCheckTagInitializationAlreadyApplied() {
		this.model = TestHelper.loadModel(this.testmodeldir, MODEL_NAME);
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		try {
			Class class1 = UMLHelper.getElementOfNameAndType(this.model, CLASS_ONE, Class.class);
			assertNotNull(class1);
			Class class2 = UMLHelper.getElementOfNameAndType(this.model, CLASS_TWO, Class.class);
			assertNotNull(class2);
			StereotypeApplication addApp2 = UMLchangeUtil.getStereotypeApplication(UMLchange.ADD, class2);
			assertNotNull(addApp2);
			for (TaggedValue tv : addApp2.getTaggedValues()) {
				assertNotNull(tv.getValue());
			}
		} catch (ModelElementNotFoundException e) {
			fail("At least one expected elemtent hasn't been found. \n " + e.getMessage());
		}
	}

	@Test
	public final void testCheckTagInitializationNewApplication() {
		this.model = TestHelper.loadModel(this.testmodeldir, MODEL_NAME);
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		try {
			Class class1 = UMLHelper.getElementOfNameAndType(this.model, CLASS_ONE, Class.class);
			assertNotNull(class1);
			Class class2 = UMLHelper.getElementOfNameAndType(this.model, CLASS_TWO, Class.class);
			assertNotNull(class2);
			StereotypeApplication addApp1 = UMLHelper.applyStereotype(class1, "UMLchange::add");
			assertNotNull(addApp1);
			for (TaggedValue tv : addApp1.getTaggedValues()) {
				assertNotNull(tv.getValue());
			}		
		} catch (ModelElementNotFoundException e) {
			fail("At least one expected elemtent hasn't been found. \n " + e.getMessage());
		}
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
}
