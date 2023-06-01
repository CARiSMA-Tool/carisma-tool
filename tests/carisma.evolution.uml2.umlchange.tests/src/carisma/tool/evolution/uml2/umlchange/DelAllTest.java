package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Ignore;
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
	@Ignore
	public final void testNoContent() {
		this.model = TestHelper.loadModel(this.filepath, "del-all.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(2, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
	}
	
	
	/**
	 * tests for stereotypes with content.
	 */
	@Test
	@Ignore
	public final void testWithContent() {
		this.model = TestHelper.loadModel(this.filepath, "del-allContent.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(1, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
	}

	/**
	 * tests if a model with no name will be renamed.
	 */
	@Test
	@Ignore
	public final void testModelRenaming() {
		this.model = TestHelper.loadModel(this.filepath, "SecureDependencies.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		assertEquals("someName", (this.model.getName()));
	}
	
	/**
	 * tests if the add-all stereotype deletes the right stereotype.
	 */
	@Test
	@Ignore
	public final void testStereotype() {
		this.model = TestHelper.loadModel(this.filepath, "SecureDependencies.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
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
		if (this.model != null) {
			TestHelper.unloadModel(this.model);
			this.model = null;
		}
	}
}
