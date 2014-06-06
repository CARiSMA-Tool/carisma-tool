package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.List;

import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Test;

import carisma.evolution.Change;
import carisma.evolution.ChangeConstraint;
import carisma.evolution.ConstraintType;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.tests.modelutils.uml.TestHelper;

public class CommonTest {

	private String testmodeldir = "resources/models/common";
	
	private UMLchangeParser parser = null;
	
	private Model model = null;
		
	@Test
	public void testNoProfileApplied() {
		model = TestHelper.loadModel(testmodeldir, "NoProfile.uml");		
		parser = new UMLchangeParser(model);
		assertNotNull(parser); 
		assertEquals(0, parser.generateDeltaDescriptions().size());
	}
	
	@Test
	public void testNoModel() {
		parser = new UMLchangeParser(null);
		assertEquals(0, parser.generateDeltaDescriptions().size());
	}
	
	@Test
	public void testProfileNoApplications() {
		model = TestHelper.loadModel(testmodeldir, "ProfileNoApplications.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser); 
		assertEquals(0, parser.generateDeltaDescriptions().size());		
	}
	
	@Test
	public void testCorrectChangeConstraints() {
		model = TestHelper.loadModel(testmodeldir, "CorrectChangeConstraints.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser); 
		List<Change> changes = parser.generateDeltaDescriptions(); 
		assertEquals(3, changes.size());
		Change addOp = null;
		Change substClass = null;
		Change delClass = null;
		for (Change change : changes) {
			for (ChangeConstraint cc : change.getConstraints()) {
				assertEquals(change, cc.getConstrainedChange());
			}
			if (change.getRef().equals("addOp")) {
				addOp = change;
				continue;
			} else if (change.getRef().equals("substClass")) {
				substClass = change;
				continue;
				
			} else if (change.getRef().equals("delClass")) {
				delClass = change;
				continue;
			} else {
				fail("Some Change with unexpected name: " + change.getRef());
			}
		}
		assertNotNull(addOp);
		assertNotNull(substClass);
		assertNotNull(delClass);
		
		assertEquals(2, addOp.getConstraints().size());
		for (ChangeConstraint cc : addOp.getConstraints()) {
			if (cc.getType() == ConstraintType.AND) {
				assertEquals(substClass, cc.getReferencedChange());
			} else if (cc.getType() == ConstraintType.NOT) {
				assertEquals(delClass, cc.getReferencedChange());
			} else {
				fail("Wrong type of constraint in change!");
			}
		}
		assertEquals(1, substClass.getConstraints().size());
		assertEquals(ConstraintType.REQ, substClass.getConstraints().get(0).getType());
		assertEquals(delClass, substClass.getConstraints().get(0).getReferencedChange());
		assertEquals(1, delClass.getConstraints().size());
		assertEquals(ConstraintType.AND, delClass.getConstraints().get(0).getType());
		assertEquals(substClass, delClass.getConstraints().get(0).getReferencedChange());
	}
	
	@Test
	public void testDuplicateConstraints() {
		model = TestHelper.loadModel(testmodeldir, "DuplicateConstraints.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser); 
		List<Change> changes = parser.generateDeltaDescriptions(); 
		assertEquals(2, changes.size());
		Change addOp = null;
		Change delClass = null;
		for (Change change : changes) {
			for (ChangeConstraint cc : change.getConstraints()) {
				assertEquals(change, cc.getConstrainedChange());
			}
			if (change.getRef().equals("addOp")) {
				addOp = change;
				continue;
			} else if (change.getRef().equals("delClass")) {
				delClass = change;
				continue;
			} else {
				fail("Some Change with unexpected name: " + change.getRef());
			}
		}
		assertNotNull(addOp);
		assertNotNull(delClass);
		assertEquals(1, addOp.getConstraints().size());
		for (ChangeConstraint cc : addOp.getConstraints()) {
			if (cc.getType() == ConstraintType.AND) {
				assertEquals(delClass, cc.getReferencedChange());
			} else {
				fail("Wrong type of constraint in change!");
			}
		}
	}
	
	@Test
	public void testWrongSyntaxConstraints() {
		model = TestHelper.loadModel(testmodeldir, "WrongSyntaxConstraints.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser); 
		List<Change> changes = parser.generateDeltaDescriptions(); 
		assertEquals(3, changes.size());
		Change addOp = null;
		Change substClass = null;
		Change delClass = null;

		for (Change change : changes) {
			if (change.getRef().equals("addOp")) {
				addOp = change;
				continue;
			} else if (change.getRef().equals("substClass")) {
				substClass = change;
				continue;
				
			} else if (change.getRef().equals("delClass")) {
				delClass = change;
				continue;
			} else {
				fail("Some Change with unexpected name: " + change.getRef());
			}
		}
				
		assertNotNull(addOp);
		assertNotNull(substClass);
		assertNotNull(delClass);
		
		assertEquals(1, addOp.getConstraints().size());
		assertEquals(ConstraintType.AND, addOp.getConstraints().get(0).getType());
		assertEquals(addOp, addOp.getConstraints().get(0).getConstrainedChange());
		assertEquals(substClass, addOp.getConstraints().get(0).getReferencedChange());
		assertEquals(0, substClass.getConstraints().size());
		assertEquals(0, delClass.getConstraints().size());
	}
	
	@Test
	public void testWrongChangeValues() {
		model = TestHelper.loadModel(testmodeldir, "WrongChangeValues.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser); 
		List<Change> changes = parser.generateDeltaDescriptions(); 
		assertEquals(0, changes.size());
	}
	
	@Test
	public void testWrongExt() {
		model = TestHelper.loadModel(testmodeldir, "WrongExt.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser); 
		List<Change> changes = parser.generateDeltaDescriptions(); 
		assertEquals(0, changes.size());				
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
