package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.evolution.Change;
import carisma.evolution.CopyElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.tests.modelutils.uml.TestHelper;

/**
 * Testclass for the copy stereotype.
 * @author Klaus Rudack
 */
public class CopyTest {

	/**
	 * the path of the directory where the testmodels lay in.
	 */
	private String filepath = "resources/models/copy";
		
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser = null;
	
	/**
	 * Test model.
	 */
	private Model model;

	/**
	 * tests if the stereotype <<copy>> will be handled correctly.
	 */
	@Test
	@Ignore
	public final void testCopyStereotypes() {
		this.model = TestHelper.loadModel(this.filepath, "testCopyValues.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(2, changeList.size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(1, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
		assertTrue(changeList.get(0).getAlternatives().get(0).getDeltaElements().get(0) instanceof CopyElement);
		CopyElement cp = (CopyElement) changeList.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		assertNotNull(cp);
		assertEquals("Property1", ((NamedElement) cp.getTarget()).getName());
		assertEquals("Class2", ((NamedElement) cp.getReceivingElement()).getName());
	}
	
	/**
	 * tests if the target of an AddElement is null if the given namespace is wrong.
	 */
	@Test
	@Ignore
	public final void testNullTarget() {
		this.model = TestHelper.loadModel(this.filepath, "testCopyValues.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(2, changeList.size());
	}
	
	/**
	 * tests if a element wont be copied if the new namespace is the same than the old and the name wont be changed.
	 */
	@Test
	@Ignore
	public final void testNamespace() {
		this.model = TestHelper.loadModel(this.filepath, "testCopySameNamespace.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(2, changeList.size());
		for (Change change : changeList) {
			if (change.getRef().equalsIgnoreCase("someRef1")) {
				assertNotNull(change);
			} else if (change.getRef().equalsIgnoreCase("someRef2")) {
				assertNotNull(change);
			} else {
				fail("someRef3 in change list.");
			}
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
