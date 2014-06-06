package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.evolution.Change;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.tests.modelutils.uml.TestHelper;


/**
 * JUnit testclass for the UMLchange-stereotype subst-all.
 * @author Klaus Rudack
 *
 */
public class SubstAllTest {

	/**
	 * the path of the directory where the testmodels lay in.
	 */
	private String filepath = "resources/models/subst-all";
	
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser = null;
	
	/**
	 * Test model.
	 */
	private Model model;
	
	/**
	 * test.
	 */
	@Test
	public final void test() {
		model = TestHelper.loadModel(filepath, "subst-all.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		assertTrue(UMLHelper.isProfileApplied(model, UMLchange.DESCRIPTOR));
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertNotNull(changeList);
		assertEquals(1, changeList.size());
		assertEquals(2, changeList.get(0).getAlternatives().size());
		assertEquals(2, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
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
