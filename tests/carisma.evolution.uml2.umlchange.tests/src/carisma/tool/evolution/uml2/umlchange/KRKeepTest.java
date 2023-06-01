package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.evolution.AddElement;
import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.tests.modelutils.uml.TestHelper;

/**
 * JUnit test for the UMLchange Stereotype <<keep>>.
 * @author Klaus Rudack
 *
 */
public class KRKeepTest {

	/**
	 * the path of the directory where the test models lay in.
	 */
	private String filepath = "resources/models/subst/keep";
		
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser = null;
	
	/**
	 * Test model.
	 */
	private Model model;
	
	/**
	 * tests the <<keep>> stereotype.
	 */
	@Test
	@Ignore
	public final void test() {
		this.model = TestHelper.loadModel(this.filepath, "ParserKeep.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		Change change = changeList.get(0);
		assertEquals(2, change.getAlternatives().size());
		Alternative firstAlt = change.getAlternatives().get(0);
		assertEquals(1, firstAlt.getDeltaElements().size());
		SubstElement subst = (SubstElement) firstAlt.getDeltaElements().get(0);
		assertEquals(2, (subst.getAllAddedElements().size()));
		AddElement firstAdd = subst.getAllAddedElements().get(0);
		assertEquals(2, firstAdd.getAllAddedElements().size());
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
