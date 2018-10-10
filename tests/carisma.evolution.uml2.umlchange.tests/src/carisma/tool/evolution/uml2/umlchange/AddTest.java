package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;
import carisma.tests.modelutils.uml.TestHelper;


public class AddTest {

	private String testmodeldir = "resources/models/add";
	
	private Model model = null;
	
	private UMLchangeParser parser = null;
		
	@Test
	public void testAddSingleElement() {
		this.model = TestHelper.loadModel(this.testmodeldir, "addSingle_Element.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		Class class1 = TestHelper.checkedGetElement(this.model, "Class1", Class.class);
		assertTrue(UMLchangeUtil.hasStereotype(UMLchange.ADD, class1));
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		Change addSomething = changes.get(0);
		assertNotNull(addSomething);
		assertEquals("addSomething", addSomething.getRef());
		assertTrue(addSomething.getConstraints().isEmpty());
		assertEquals(1, addSomething.getAlternatives().size());
		Alternative alt = addSomething.getAlternatives().get(0);
		assertNotNull(alt);
		assertEquals(1, alt.getDeltaElements().size());
		AddElement de = (AddElement) alt.getDeltaElements().get(0);
		assertEquals(class1, de.getTarget());
		assertEquals("Operation", de.getMetaClass().getName());
		assertEquals(1, de.getValues().size());
		assertEquals("doHans", de.getValues().get("name"));
		assertEquals(0, de.getContent().size());
		assertEquals(1, de.getAllAddedElements().size());
	}
	
	@Test
	public void testNonExistingMetaclass() {
		this.model = TestHelper.loadModel(this.testmodeldir, "addSingle_NonExistingMetaclass.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		assertEquals(0, this.parser.generateDeltaDescriptions().size());
	}
	
	@Test
	public void testSimpleWithContent() {
		this.model = TestHelper.loadModel(this.testmodeldir, "addSingle_WithContent.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		Change change = changes.get(0);
		assertEquals(1, change.getAlternatives().size());
		Alternative alt = change.getAlternatives().get(0);
		assertEquals(1, alt.getDeltaElements().size());
		AddElement de = (AddElement) alt.getDeltaElements().get(0);
		assertEquals(2, de.getContent().size());
		assertEquals(3, de.getAllAddedElements().size());
		assertEquals(1, de.getValues().size());		
		for (AddElement containedAdd : de.getContent()) {
			assertEquals("Operation", containedAdd.getMetaClass().getName());
			assertTrue(containedAdd.getContent().isEmpty());
			assertEquals(1, containedAdd.getValues().size());
			assertTrue(containedAdd.getValues().containsKey("name"));
		}
	}
	
	@Test
	public void testSimpleWithContentThreeLevels() {
		this.model = TestHelper.loadModel(this.testmodeldir, "addSingle_WithContentThreeLevels.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		Change change = changes.get(0);
		assertEquals(1, change.getAlternatives().size());
		Alternative alt = change.getAlternatives().get(0);
		assertEquals(1, alt.getDeltaElements().size());
		AddElement de = (AddElement) alt.getDeltaElements().get(0);
		assertEquals(1, de.getContent().size());
		assertEquals(3, de.getAllAddedElements().size());
		assertEquals(1, de.getValues().size());
		AddElement levelTwo = de.getContent().get(0);
		assertEquals(1, levelTwo.getContent().size());
		assertEquals(2, levelTwo.getAllAddedElements().size());
		assertEquals(1, levelTwo.getValues().size());
		AddElement levelThree = levelTwo.getContent().get(0);
		assertEquals("Parameter", levelThree.getMetaClass().getName());
		assertEquals(1, levelThree.getValues().size());
		assertTrue(levelThree.getValues().containsValue("LevelThree"));
		assertEquals(0, levelThree.getContent().size());
		assertEquals(1, levelThree.getAllAddedElements().size());
		assertEquals(1, levelThree.getValues().size());
	}

	@Test
	public void testSimpleContentNonExistingMetaclass() {
		this.model = TestHelper.loadModel(this.testmodeldir, "addSingle_ContentNonExistingMetaclass.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		Change change = changes.get(0);
		assertEquals(1, change.getAlternatives().size());
		Alternative alt = change.getAlternatives().get(0);
		assertEquals(1, alt.getDeltaElements().size());
		AddElement de = (AddElement) alt.getDeltaElements().get(0);
		assertEquals(1, de.getContent().size());
		assertEquals(2, de.getAllAddedElements().size());
		assertEquals(1, de.getValues().size());
		AddElement addParameter = de.getContent().get(0);
		assertEquals(0, addParameter.getContent().size());
		assertEquals(1, addParameter.getAllAddedElements().size());
		assertEquals(1, addParameter.getValues().size());
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
