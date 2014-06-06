package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.DelElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.tests.modelutils.uml.TestHelper;


/**
 * JUnit testclass for the UMLchange-stereotype del-all.
 * @author Klaus Rudack
 *
 */
public class DelAllTest {

	/**
	 * the path of the directory where the testmodels lay in.
	 */
	private String filepath = "resources/models/del-all";
		
	/**
	 * the original model.
	 */
	private Model model = null;
	
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser = null;
		
	/**
	 * tests for stereotypes with no content.
	 */
	@Test
	public final void testNoContent() {
		model = TestHelper.loadModel(filepath, "del-all.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(2, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
	}
	
	
	/**
	 * tests for stereotypes with content.
	 */
	@Test
	public final void testWithContent() {
		model = TestHelper.loadModel(filepath, "del-allContent.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(1, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
	}

	/**
	 * tests if a model with no name will be renamed.
	 */
	@Test
	public final void testModelRenaming() {
		model = TestHelper.loadModel(filepath, "SecureDependencies.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		assertEquals("someName", (model.getName()));
	}
	
	/**
	 * tests if the add-all stereotype deletes the right stereotype.
	 */
	@Test
	public final void testStereotype() {
		model = TestHelper.loadModel(filepath, "SecureDependencies.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertNotNull(changeList);
		assertEquals(1, changeList.size());
		Change change = changeList.get(0);
		assertNotNull(change);
		assertEquals(1, change.getAlternatives().size());
		Alternative alt = change.getAlternatives().get(0);
		assertNotNull(alt);
		assertEquals(1, alt.getDeltaElements().size());
		DelElement del = (DelElement) alt.getDeltaElements().get(0);
		assertNotNull(del);
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
