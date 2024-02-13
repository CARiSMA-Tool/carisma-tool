package carisma.check.activitypaths;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.uml2.uml.ActivityFinalNode;
import org.eclipse.uml2.uml.ControlFlow;
import org.eclipse.uml2.uml.DecisionNode;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.ForkNode;
import org.eclipse.uml2.uml.InitialNode;
import org.eclipse.uml2.uml.JoinNode;
import org.eclipse.uml2.uml.MergeNode;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Package;
import org.junit.After;
import org.junit.Test;

import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;


public class ActivitypathsTest {
	
	private String filepath = "resources/models/";
	
	private Model model;
	
	@Test
	public void testFalse() {
		this.model = TestHelper.loadModel(this.filepath, "testActivityFail.uml");
		ActivityDiagramManager adm = new ActivityDiagramManager(this.model);
		assertEquals(0, adm.getAllPaths().size());
	}
	
	@Test
	public void testTrue() {
		this.model = TestHelper.loadModel(this.filepath, "testActivitySuccess.uml");
		ActivityDiagramManager adm = new ActivityDiagramManager((Package) (this.model).getOwnedElements().get(0));
		boolean b = false;
		if (adm.getAllPaths().size() > 0) {
			b = true;
		}
		assertTrue(b);
		b = true;
		List<InitialNode> l = UMLHelper.getAllElementsOfType(this.model, InitialNode.class);
		if (l.size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element initalNode  : UMLHelper.getAllElementsOfType(this.model, InitialNode.class)) {
			assertTrue(ActivityDiagramManager.isInitialNode(initalNode));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(this.model, ActivityFinalNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element finalNode  : UMLHelper.getAllElementsOfType(this.model, ActivityFinalNode.class)) {
			assertTrue(ActivityDiagramManager.isFinalNode(finalNode));
			assertFalse(ActivityDiagramManager.isForkNode(finalNode));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(this.model, ControlFlow.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element edge  : UMLHelper.getAllElementsOfType(this.model, ControlFlow.class)) {
			assertTrue(ActivityDiagramManager.isEdge(edge));
			assertFalse(ActivityDiagramManager.isForkNode(edge));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(this.model, DecisionNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element dNode  : UMLHelper.getAllElementsOfType(this.model, DecisionNode.class)) {
			assertTrue(ActivityDiagramManager.isDecisionNode(dNode));
			assertFalse(ActivityDiagramManager.isForkNode(dNode));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(this.model, MergeNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element mNode  : UMLHelper.getAllElementsOfType(this.model, MergeNode.class)) {
			assertTrue(ActivityDiagramManager.isMergeNode(mNode));
			assertFalse(ActivityDiagramManager.isForkNode(mNode));
		}
	}
	
	@Test
	public void testForkJoin() {
		this.model = TestHelper.loadModel(this.filepath, "testActivityForkJoin.uml");
		ActivityDiagramManager adm = new ActivityDiagramManager((Package) ((Element) this.model).getOwnedElements().get(0));
		boolean b = false;
		if (adm.getAllPaths().size() > 0) {
			b = true;
		}
		assertTrue(b);
		b = true;
		if (UMLHelper.getAllElementsOfType(this.model, ForkNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element forkNode  : UMLHelper.getAllElementsOfType(this.model, ForkNode.class)) {
			assertTrue(ActivityDiagramManager.isForkNode(forkNode));
			assertFalse(ActivityDiagramManager.isJoinNode(forkNode));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(this.model, JoinNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element joinNode  : UMLHelper.getAllElementsOfType(this.model, JoinNode.class)) {
			assertTrue(ActivityDiagramManager.isJoinNode(joinNode));
			assertFalse(ActivityDiagramManager.isForkNode(joinNode));
		}
	}
	
	@SuppressWarnings("static-method")
	@Test(expected = NullPointerException.class)
	public final void testNull() {
		@SuppressWarnings("unused")
        ActivityDiagramManager adm = new ActivityDiagramManager(null);
	}
	
	@After
	public final void cleanUp() {
		if (this.model != null) {
			TestHelper.unloadModel(this.model);
			this.model = null;
		}
	}
}
