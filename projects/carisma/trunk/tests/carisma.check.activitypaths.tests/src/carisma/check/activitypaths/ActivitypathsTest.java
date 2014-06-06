package carisma.check.activitypaths;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
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

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.tests.modelutils.uml.TestHelper;


public class ActivitypathsTest {
	
	private String filepath = "resources/models/";
	
	private Model model;
	
	@Test
	public void testFalse() {
		model = TestHelper.loadModel(filepath, "testActivityFail.uml");
		ActivityDiagramManager adm = new ActivityDiagramManager(model);
		assertEquals(0, adm.getAllPaths().size());
	}
	
	@Test
	public void testTrue() {
		model = TestHelper.loadModel(filepath, "testActivitySuccess.uml");
		ActivityDiagramManager adm = new ActivityDiagramManager((Package) (model).getOwnedElements().get(0));
		boolean b = false;
		if (adm.getAllPaths().size() > 0) {
			b = true;
		}
		assertTrue(b);
		b = true;
		List<InitialNode> l = UMLHelper.getAllElementsOfType(model, InitialNode.class);
		if (l.size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element initalNode  : UMLHelper.getAllElementsOfType(model, InitialNode.class)) {
			assertTrue(adm.isInitialNode(initalNode));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(model, ActivityFinalNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element finalNode  : UMLHelper.getAllElementsOfType(model, ActivityFinalNode.class)) {
			assertTrue(adm.isFinalNode(finalNode));
			assertFalse(adm.isForkNode(finalNode));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(model, ControlFlow.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element edge  : UMLHelper.getAllElementsOfType(model, ControlFlow.class)) {
			assertTrue(adm.isEdge(edge));
			assertFalse(adm.isForkNode(edge));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(model, DecisionNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element dNode  : UMLHelper.getAllElementsOfType(model, DecisionNode.class)) {
			assertTrue(adm.isDecisionNode(dNode));
			assertFalse(adm.isForkNode(dNode));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(model, MergeNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element mNode  : UMLHelper.getAllElementsOfType(model, MergeNode.class)) {
			assertTrue(adm.isMergeNode(mNode));
			assertFalse(adm.isForkNode(mNode));
		}
	}
	
	@Test
	public void testForkJoin() {
		model = TestHelper.loadModel(filepath, "testActivityForkJoin.uml");
		ActivityDiagramManager adm = new ActivityDiagramManager((Package) ((Element) model).getOwnedElements().get(0));
		boolean b = false;
		if (adm.getAllPaths().size() > 0) {
			b = true;
		}
		assertTrue(b);
		b = true;
		if (UMLHelper.getAllElementsOfType(model, ForkNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element forkNode  : UMLHelper.getAllElementsOfType(model, ForkNode.class)) {
			assertTrue(adm.isForkNode(forkNode));
			assertFalse(adm.isJoinNode(forkNode));
		}
		b = true;
		if (UMLHelper.getAllElementsOfType(model, JoinNode.class).size() == 0) {
			b = false;
		}
		assertTrue(b);
		for (Element joinNode  : UMLHelper.getAllElementsOfType(model, JoinNode.class)) {
			assertTrue(adm.isJoinNode(joinNode));
			assertFalse(adm.isForkNode(joinNode));
		}
	}
	
	@Test(expected = NullPointerException.class)
	public final void testNull() {
		@SuppressWarnings("unused")
        ActivityDiagramManager adm = new ActivityDiagramManager(null);
	}
	
	@After
	public final void cleanUp() {
		if (model != null) {
			TestHelper.unloadModel(model);
			model = null;
		}
	}
}
