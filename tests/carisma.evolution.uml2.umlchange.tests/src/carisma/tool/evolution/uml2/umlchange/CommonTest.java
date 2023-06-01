package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.List;

import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Ignore;
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
		this.model = TestHelper.loadModel(this.testmodeldir, "NoProfile.uml");		
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		assertEquals(0, this.parser.generateDeltaDescriptions().size());
	}
	
	@Test
	public void testNoModel() {
		this.parser = new UMLchangeParser(null);
		assertEquals(0, this.parser.generateDeltaDescriptions().size());
	}
	
	@Test
	public void testProfileNoApplications() {
		this.model = TestHelper.loadModel(this.testmodeldir, "ProfileNoApplications.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		assertEquals(0, this.parser.generateDeltaDescriptions().size());		
	}
	
	@Test
	public void testCorrectChangeConstraints() {
		this.model = TestHelper.loadModel(this.testmodeldir, "CorrectChangeConstraints.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions(); 
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
		this.model = TestHelper.loadModel(this.testmodeldir, "DuplicateConstraints.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions(); 
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
		this.model = TestHelper.loadModel(this.testmodeldir, "WrongSyntaxConstraints.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions(); 
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
	@Ignore
	public void testWrongChangeValues() {
		this.model = TestHelper.loadModel(this.testmodeldir, "WrongChangeValues.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions(); 
		assertEquals(0, changes.size());
	}
	
	@Test
	public void testWrongExt() {
		this.model = TestHelper.loadModel(this.testmodeldir, "WrongExt.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions(); 
		assertEquals(0, changes.size());				
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
