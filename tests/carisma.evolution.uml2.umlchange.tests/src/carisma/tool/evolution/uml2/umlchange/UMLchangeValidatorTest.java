package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Model;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.uml2.umlchange.UMLchangeValidator;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;
import carisma.tests.modelutils.uml.TestHelper;



/**
 * JUnit test class for the UMLchangeValidator.
 * @author Klaus Rudack
 *
 */
public class UMLchangeValidatorTest {

	/**
	 * path to the test model folder.
	 */
	private String filepath = "resources/models/validator";
				
	/**
	 * the model.
	 */
	private Model model = null;
	
	/**
	 * UML model path for UML_CHANGE_VALIDATOR.
	 */
	private static final String UML_CHANGE_VALIDATOR = "testUMLchangeValidator.uml";
	
	/**
	 * Constant String for the name of Tag 'new'.
	 */
	private static final String NEW = "new";
	
	private UMLchangeValidator validator = null;
		
	@Before
	public final void init() {
		this.validator = new UMLchangeValidator();
	}
	
//	/**
//	 * this test tests the correctness of refId's and KeyValuePairs.
//	 */
//	@Test
//	public final void testKeyValues() {
//		
//		assertTrue(UMLchangeValidator.validUMLchangeGrammar("someRef={(name=newName)}"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef=(name=newName)"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef="));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={name=newName,}"));
//		
//		assertTrue(UMLchangeValidator.validUMLchangeGrammar("someRef={name=newName,visibilty=private}"));
//		assertTrue(UMLchangeValidator.validUMLchangeGrammar("someRef={name=newName,visibilty=private,isLeaf=true}"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={name=newName, visibilty=private}"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={name=newName,}"));
//		
//
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef=(name=newName)"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={(name=newName,visibilty=private)}"));
//		
//	}
//	
//	/**
//	 * this test tests the correctness of alternatives.
//	 */
//	@Test
//	public final void testAlternatives() {
//		
//		assertTrue(UMLchangeValidator.validUMLchangeGrammar("someRef={name=newName,visibilty=private,isLeaf=true},{name=otherName,visibilty=pacakged}"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={name=newName,visibilty=private,isLeaf=true},"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={Class"
//				+ "(name=newName,visibilty=private,isLeaf=true)},{name=otherName,visibilty=packaged}"));
//		
//	}
//	
//	/**
//	 * this test tests the correctness of "contents".
//	 */
//	@Test
//	public final void testContents() {
//		
//		assertTrue(UMLchangeValidator.validUMLchangeGrammar("someRef={name=newName,contents=<Stereotype(name=UMLchange.del)>}"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={contents=<Stereotype(name=UMLchange.del>"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={contents=<(name=UMLchange.del>}"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammar("someRef={contents=<Stereotype(name=UMLchange.del)}"));
//		
//	}
//	
//	/**
//	 * this test tests grammar expressions with a metaclass for e.g. new.
//	 */
//	@Test
//	public final void testWithMetaclass() {
//		
//		assertTrue(UMLchangeValidator.validUMLchangeGrammarWithMetaclass("someRef={Class(name=newName,visibilty=private,isLeaf=true)}"));
//		assertTrue(UMLchangeValidator.validUMLchangeGrammarWithMetaclass("someRef={Class"
//				+ "(name=newName,visibilty=private,isLeaf=true)},{Package(name=otherName,visibilty=packaged)}"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammarWithMetaclass("someRef={name=hallo}"));
//		
//	}
//	
//	/**
//	 * 
//	 */
//	@Test
//	public final void testExtGrammar() {
//		
//		assertTrue(UMLchangeValidator.validUMLchangeGrammarExt("someRef=Stereotype.Tag"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammarExt("someRef={Stereotype.Tag}"));
//		assertFalse(UMLchangeValidator.validUMLchangeGrammarExt("Stereotype.Tag"));
//		
//	}
//	
//	/**
//	 * this test tests if the refId's in the new/edit etc. tag of a stereotype are set in the refTag.
//	 */
//	@Test
//	public final void testStereotypeRefID() {
//		loadModel("validator/singleAddStereotype.uml");
//		String stereoNameAdds = "add";
//		String stereoNameEdit = "edit";
//		Class class1 = null;
//		try {
//			class1 = (Class) UMLHelper.getElementByName(model, "Class1");
//		} catch (ModelElementNotFoundException e) {
//			Logger.log(LogLevel.ERROR, e.getMessage(), e);
//			fail(e.getMessage());
//		}
//		assertNotNull(class1);
//		StereotypeApplication stereoAppAdd = UMLHelper.getStereotypeApplication(class1, stereoNameAdds);
//		assertNotNull(stereoAppAdd);
//		stereoAppAdd.getTaggedValue("ref").setValue("someRef");
//		stereoAppAdd.getTaggedValue(NEW).setValue("someRef={name=new}");
//		assertTrue(UMLchangeValidator.validStereotypeTags(stereoAppAdd, null));
//		stereoAppAdd.getTaggedValue(NEW).setValue("otherRef={name=new}");
//		assertFalse(UMLchangeValidator.validStereotypeTags(stereoAppAdd, null));
//		stereoAppAdd.getTaggedValue("ref").setValue("otherRef");
//		assertTrue(UMLchangeValidator.validStereotypeTags(stereoAppAdd, null));
//		stereoAppAdd.getTaggedValue(NEW).setValue("otherRef{name=new}");
//		assertFalse(UMLchangeValidator.validStereotypeTags(stereoAppAdd, null));
//		
//		Class class2 = null;
//		try {
//			class2 = (Class) UMLHelper.getElementByName(model, "Class2");
//		} catch (ModelElementNotFoundException e) {
//			Logger.log(LogLevel.ERROR, e.getMessage(), e);
//			fail(e.getMessage());
//		}
//		assertNotNull(class2);
//		StereotypeApplication stereoAppEdit = UMLHelper.getStereotypeApplication(class2, stereoNameEdit);
//		assertNotNull(stereoAppEdit);
//		stereoAppEdit.getTaggedValue("ref").setValue("refID");
//		stereoAppEdit.getTaggedValue("values").setValue("refID={name=newName}");
//		assertTrue(UMLchangeValidator.validStereotypeTags(stereoAppEdit, null));
//		stereoAppEdit.getTaggedValue("values").setValue("otherID={name=otherName}");
//		assertFalse(UMLchangeValidator.validStereotypeTags(stereoAppEdit, null));
//	}
//	
//	/**
//	 * this tests test the main validator method to validate a complete model.
//	 */
//	@Test
//	public final void testFullModel() {
//		loadModel("validator/singleAddStereotype.uml");
//		String stereoNameAdds = "add";
//		String stereoNameEdit = "edit";
//		Class class1 = null;
//		try {
//			class1 = (Class) UMLHelper.getElementByName(model, "Class1");
//		} catch (ModelElementNotFoundException e) {
//			Logger.log(LogLevel.ERROR, e.getMessage(), e);
//			fail(e.getMessage());
//		}
//		assertNotNull(class1);
//		StereotypeApplication stereoAppAdd = UMLHelper.getStereotypeApplication(class1, stereoNameAdds);
//		assertNotNull(stereoAppAdd);
//		stereoAppAdd.getTaggedValue("ref").setValue("someRef");
//		stereoAppAdd.getTaggedValue(NEW).setValue("someRef={name=new}");	
//		stereoAppAdd.getTaggedValue("ref").setValue("otherRef");
//		stereoAppAdd.getTaggedValue(NEW).setValue("otherRef={name=new}");
//
//		Class class2 = null;
//		try {
//			class2 = (Class) UMLHelper.getElementByName(model, "Class2");
//		} catch (ModelElementNotFoundException e) {
//			Logger.log(LogLevel.ERROR, e.getMessage(), e);
//			fail(e.getMessage());
//		}
//		assertNotNull(class2);
//		StereotypeApplication stereoAppEdit = UMLHelper.getStereotypeApplication(class2, stereoNameEdit);
//		assertNotNull(stereoAppEdit);
//		stereoAppEdit.getTaggedValue("ref").setValue("refID");
//		stereoAppEdit.getTaggedValue("values").setValue("refID={name=newName}");
//		
//		assertTrue(UMLchangeValidator.validModel(model, null));
//	}
//	
	/**
	 * this tests tests the method to check if a given String is a valid UMLchange constraint.
	 */
	@Test
	public final void testConstraint() {
		//TODO: Implement test	
	}
	
	/**
	 * this tests tests the method to check if a given String is a valid UMLchange ext string.
	 */
	@Test
	public final void testExt() {
		//TODO: Implement test	
	}
	
	/**
	 * this tests tests the method to check if a given String is a valid UMLchange new string.
	 */
	@Test
	public final void testNew() {
		//TODO: Implement test	
	}
	
	/**
	 * this tests tests the UMLchangeValidator with a complete model.
	 */
	@Test
	@Ignore
	public final void testModel() {
		this.model = TestHelper.loadModel(this.filepath, UML_CHANGE_VALIDATOR);
		assertTrue(this.validator.isValidModel(this.model));
	}
	
	@Test
	@Ignore
	public final void testInvalidKeep() {
		this.model = TestHelper.loadModel(this.filepath, UML_CHANGE_VALIDATOR);
		Class keepClass = null;
		try {
			keepClass = (Class) UMLHelper.getElementByName(this.model, "Class3");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "At least one expected elemtent hasn't been found.", e);
			fail("Couln't find ModelElement Class3.");
		}
		assertNotNull(keepClass);
		StereotypeApplication stereoApp = UMLchangeUtil.getStereotypeApplication(UMLchange.KEEP, keepClass);
		assertNotNull(stereoApp);
		stereoApp.getTaggedValue("adopter").setValue("");
		assertFalse(this.validator.isValidModel(this.model));
	}
	

	@Test
	@Ignore
	public final void testInvalidDelRef() {
		this.model = TestHelper.loadModel(this.filepath, UML_CHANGE_VALIDATOR);
		Class delClass = null;
		try {
			delClass = (Class) UMLHelper.getElementByName(this.model, "Class1");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "At least one expected elemtent hasn't been found.", e);
			fail("Couln't find ModelElement Class1.");
		}
		assertNotNull(delClass);
		StereotypeApplication stereoApp = UMLchangeUtil.getStereotypeApplication(UMLchange.DEL, delClass);
		assertNotNull(stereoApp);
		stereoApp.getTaggedValue("constraint").setValue("delRef=AND(invalidRef)");
		assertFalse(this.validator.isValidModel(this.model));
	}
	
	@Test
	@Ignore
	public final void testInvalidPattern() {
		this.model = TestHelper.loadModel(this.filepath, UML_CHANGE_VALIDATOR);
		Class patternClass = null;
		try {
			patternClass = (Class) UMLHelper.getElementByName(this.model, "Class7");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "At least one expected elemtent hasn't been found.", e);
			fail("Couln't find ModelElement Class7.");
		}
		assertNotNull(patternClass);
		StereotypeApplication stereoApp = UMLchangeUtil.getStereotypeApplication(UMLchange.ADDALL, patternClass);
		assertNotNull(stereoApp);
		stereoApp.getTaggedValue("pattern").setValue("addAllRef=this.pattern.is.invalid");
		assertFalse(this.validator.isValidModel(this.model));
	}
	
	
	@Test
	public final void testContentsPairs() {
		//TODO: Implement test	
	}
	
	/**
	 * tests if the validator recognizes weather the content of the ext-value really exists or not.
	 */
	@Test
	public final void testExtContent() {
		this.model = TestHelper.loadModel(this.filepath, "testValidatorExt.uml");
	}
	
	@Test
	public final void testConstraints() {
		List<String> valids = new ArrayList<>();
		valids.add("changeID");
		valids.add("validID");
		valids.add("correctID");
		String value = "changeID=AND(otherID)";
		assertTrue(this.validator.hasInvalidReferences(value, valids));
		value = "changeID=AND(validID)";
		assertFalse(this.validator.hasInvalidReferences(value, valids));
		value = "changeID=AND(validID)NOT(invalidID)";
		assertTrue(this.validator.hasInvalidReferences(value, valids));
	}
	
	@Test
	@Ignore
	public final void testStereoExt() {
		this.model = TestHelper.loadModel(this.filepath, UML_CHANGE_VALIDATOR);
		Class c1 = null;
		try {
			c1 = (Class) UMLHelper.getElementByName(this.model, "Class1");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(c1);
		StereotypeApplication delApp = UMLchangeUtil.getStereotypeApplication(UMLchange.DEL, c1);
		assertNotNull(delApp);
		delApp.getTaggedValue("ext").setValue("b.u.l.l.s.h.i.t.");
		assertFalse(this.validator.isValidApplication(delApp, new ArrayList<String>()));
	}
	
	/**
	 * test for the subst-stereotype.
	 */
	@Test
	@Ignore
	public final void testSubst() {
		this.model = TestHelper.loadModel(this.filepath, UML_CHANGE_VALIDATOR);
		Class substClass = null;
		try {
			substClass = (Class) UMLHelper.getElementByName(this.model, "testUMLchangeValidator::Class9");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(substClass);
		StereotypeApplication substApp = UMLchangeUtil.getStereotypeApplication(UMLchange.SUBST, substClass);
		assertNotNull(substApp);
		assertTrue(this.validator.isValidApplication(substApp, new ArrayList<String>()));
		substApp.getTaggedValue(NEW).removeValue();
		substApp.getTaggedValue(NEW).setValue("oldRef={@invalidNamespace}");
		assertFalse(this.validator.isValidApplication(substApp, new ArrayList<String>()));
		substApp.getTaggedValue(NEW).removeValue();
		substApp.getTaggedValue(NEW).setValue("oldRef={@oldName},{@oldName}");
		assertTrue(this.validator.isValidApplication(substApp, new ArrayList<String>()));
		substApp.getTaggedValue(NEW).removeValue();
		substApp.getTaggedValue(NEW).setValue("oldRef={@oldName},{@invalidNamespace}");
		assertFalse(this.validator.isValidApplication(substApp, new ArrayList<String>()));
		Class substClass2 = null;
		try {
			substClass2 = (Class) UMLHelper.getElementByName(this.model, "testUMLchangeValidator::Class4");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(substClass2);
		StereotypeApplication substApp2 = UMLchangeUtil.getStereotypeApplication(UMLchange.SUBST, substClass2);
		assertNotNull(substApp2);
		List<String> refList = new ArrayList<>();
		refList.add("substRef");
		refList.add("delRef");
		assertTrue(this.validator.isValidApplication(substApp2, refList));
	}
	
	/**
	 * test for the old-stereotype.
	 */
	@Test
	@Ignore
	public final void testOld() {
		this.model = TestHelper.loadModel(this.filepath, UML_CHANGE_VALIDATOR);
		Class oldClass = null;
		try {
			 oldClass = (Class) UMLHelper.getElementByName(this.model, "oldName::Class9");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(oldClass);
		StereotypeApplication oldApp = UMLchangeUtil.getStereotypeApplication(UMLchange.OLD,  oldClass);
		assertNotNull(oldApp);
		assertTrue(this.validator.isValidApplication(oldApp, new ArrayList<String>()));
		Class class9 = null;
		try {
			class9 = (Class) UMLHelper.getElementByName(this.model, "testUMLchangeValidator::Class9");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(class9);
		class9.destroy();
		assertFalse(this.validator.isValidApplication(oldApp, new ArrayList<String>()));
	}
 
}
