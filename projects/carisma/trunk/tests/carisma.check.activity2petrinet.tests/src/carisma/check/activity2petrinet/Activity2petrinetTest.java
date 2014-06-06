package carisma.check.activity2petrinet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.ControlNode;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.OpaqueAction;
import org.junit.After;
import org.junit.Test;

import carisma.check.activity2petrinet.petriNet.Arc;
import carisma.check.activity2petrinet.petriNet.PetriNet;
import carisma.check.activity2petrinet.petriNet.Place;
import carisma.check.activity2petrinet.petriNet.Transition;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.modeltype.uml2.UMLHelper;
import carisma.tests.modelutils.uml.TestHelper;


/**
 * JUnit test-class for carisma.check.activity2petrinet.
 * @author Klaus Rudack
 *
 */
public class Activity2petrinetTest {

	/**
	 * path to the model-folder.
	 */
	private String filepath = "resources/models/";
	
	/**
	 * the model for the test.
	 */
	private Model model;
		
	/**
	 * this test tests if a petrinet will be returned by the Convert constructor.
	 */
	@Test
	public final void testCorrect() {
		model = TestHelper.loadModel(filepath, "activity2petrinetSuccess.uml");
		assertNotNull(model);
		PetriNet petriNet = new Convert(model).convert();
		assertNotNull(petriNet);
		List<Place> places = petriNet.getPlaces();
		List<String> placeNames = new  ArrayList<String>();
		List<Arc> arcs = petriNet.getArcs();
		List<Transition> transitions = petriNet.getTransitions();
		List<String> transitionNames = new ArrayList<String>();
		for (Place place : places) {
			placeNames.add(place.getName());
		}
		for (Transition transition : transitions) {
			transitionNames.add(transition.getName());
		}
		assertEquals("Wrong number of places", 4, places.size());
		for (ControlNode controlNodes : UMLHelper.getAllElementsOfType(model, ControlNode.class)) {
			assertTrue(placeNames.contains(controlNodes.getName()));
		}
		assertEquals("Wrong number of arcs", 8, arcs.size());
		assertEquals("Wrong number of transitions", 4, transitions.size());
		for (OpaqueAction opaqueAction : UMLHelper.getAllElementsOfType(model, OpaqueAction.class)) {
			assertTrue(transitionNames.contains(opaqueAction.getName()));
		}
	}
	
	/**
	 * this test tests if the plugin works correct with an empty model.
	 */
	@Test
	public final void testEmptyModel() {
		model = TestHelper.loadModel(filepath, "activity2petrinetEmptyModel.uml");
		assertNotNull(model);
		PetriNet petriNet = new Convert(model).convert();
		assertNotNull(petriNet);
		assertEquals(0, petriNet.getArcs().size());
		assertEquals(0, petriNet.getPlaces().size());
		assertEquals(0, petriNet.getTransitions().size());
	}
	
	/**
	 * this test tests if the plugin works correct if the model is no activity diagram.
	 */
	@Test
	public final void testNoActivityDiagram() {		
		model = TestHelper.loadModel(filepath, "activity2petrinetNoActivityDiagram.uml");
		assertNotNull(model);
		PetriNet petriNet = new Convert(model).convert();
		assertNotNull(petriNet);
		assertEquals(0, petriNet.getArcs().size());
		assertEquals(0, petriNet.getPlaces().size());
		assertEquals(0, petriNet.getTransitions().size());
	}
	
	/**
	 * cleaning up.
	 */
	@After
	public final void cleanUp() {
		TestHelper.unloadModel(model);
		model = null;
	}

}
