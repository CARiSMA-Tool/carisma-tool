package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.uml2.uml.Association;
import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.evolution.Change;
import carisma.evolution.DelElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.tests.modelutils.uml.TestHelper;

public class DelTest {

	/**
	 * the path of the directory where the test models lay in.
	 */
	private String testmodeldir = "resources/models/del";
	
	/**
	 * the original model.
	 */
	private Model model = null;
	
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser = null;
	
	/**
	 * AccompanyingDeletions contains:
	 * <<critical>>, one filled tag value,
	 * the two role ends, the four numbers (lower, upper, source, target Bound),
	 * and the dependency.
	 */
	@Test
	public final void testDelSingleElement() {
		model = TestHelper.loadModel(testmodeldir, "Del_SingleElement.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		assertTrue(UMLHelper.isProfileApplied(model, UMLchange.DESCRIPTOR));
		List<Change> changes = parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		DelElement delClass = (DelElement) changes.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		Class class1 = TestHelper.checkedGetElement(model, "Class1", Class.class);
		assertEquals(class1, delClass.getTarget());
		assertEquals(9, delClass.getAccompanyingDeletions().size());
	}
	
	@Test
	public void testDelSingleWithStereotype() {
		model = TestHelper.loadModel(testmodeldir, "Del_SingleWithStereotype.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		assertTrue(UMLHelper.isProfileApplied(model, UMLchange.DESCRIPTOR));
		List<Change> changes = parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		DelElement delClass = (DelElement) changes.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		Class class1 = TestHelper.checkedGetElement(model, "Class1", Class.class);
		assertEquals(class1, delClass.getTarget());
		assertEquals(2, delClass.getAccompanyingDeletions().size());
	}
	
	/**
	 * AccompanyingDeletions contains:
	 * <<guarded>>, the two role ends, the four numbers (lower, upper, source, target Bound).
	 */
	@Test
	public final void testDelSingleAssociation() {
		model = TestHelper.loadModel(testmodeldir, "Del_SingleAssociation.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		assertTrue(UMLHelper.isProfileApplied(model, UMLchange.DESCRIPTOR));
		List<Change> changes = parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		DelElement delClass = (DelElement) changes.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		Association assC1C2 = TestHelper.checkedGetElement(model, "Ass_C1C2", Association.class);
		assertEquals(assC1C2, delClass.getTarget());
		assertEquals(7, delClass.getAccompanyingDeletions().size());
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
