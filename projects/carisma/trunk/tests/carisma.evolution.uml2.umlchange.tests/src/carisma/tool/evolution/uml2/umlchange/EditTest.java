package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.evolution.Change;
import carisma.evolution.EditElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.tests.modelutils.uml.TestHelper;


/**
 * JUnit-test for the new edit stereotype.
 * @author Klaus Rudack
 *
 */
public class EditTest {

	/**
	 * the path of the directory where the test models lay in.
	 */
	private String filepath = "resources/models/edit";
		
	/**
	 * Constant variable for Attribute name of an AddElement.
	 */
	private static final String NAME = "name";
	
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser = null;
	
	/**
	 * Test model.
	 */
	private Model model;
		
	/**
	 * test for an class-diagram.
	 * this test tests strings
	 */
	@Test
	public final void testEditStereotype() {
		model = TestHelper.loadModel(filepath, "testEditParser.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		assertTrue(UMLHelper.isProfileApplied(model, UMLchange.DESCRIPTOR));
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertEquals(2, changeList.size());
		
		Map<String, Object> edits = ((EditElement) changeList.get(0).getAlternatives().get(0).getDeltaElements().get(0)).getValues();
		Set<String> keySet = edits.keySet();
		assertEquals(2, keySet.size());
		assertTrue(keySet.contains(NAME));
		assertEquals("newName", edits.get(NAME));
		assertTrue(keySet.contains("visibility"));
		assertEquals("private", edits.get("visibility"));

		
		Map<String, Object> edits2 = ((EditElement) changeList.get(1).getAlternatives().get(0).getDeltaElements().get(0)).getValues();
		Set<String> keySet2 = edits2.keySet();
		assertEquals(1, keySet2.size());
		assertTrue(keySet.contains(NAME));
		assertEquals("tuedeluedelu", edits2.get(NAME));
	}
	
	
	/**
	 * FIXME: Do not have to test ext usage in <<edit>> tests. (tested in CommonTest)
	 */
	@Test
	public final void testNullTarget() {
		model = TestHelper.loadModel(filepath, "testEditParser.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		assertTrue(UMLHelper.isProfileApplied(model, UMLchange.DESCRIPTOR));
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertEquals(2, changeList.size());		
	}
	
	@Test
	public final void testWrongValueSyntax() {
		model = TestHelper.loadModel(filepath, "testEditWrongValueSyntax.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		assertTrue(UMLHelper.isProfileApplied(model, UMLchange.DESCRIPTOR));
		List<Change> changes = parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		Change ch = changes.get(0);
		EditElement edit = (EditElement) ch.getAlternatives().get(0).getDeltaElements().get(0);
		assertEquals("Meier", edit.getValues().get(NAME));
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