package carisma.check.activity2petrinet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.uml2.uml.ControlNode;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.OpaqueAction;
import org.junit.After;
import org.junit.Test;

import carisma.check.activity2petrinet.petriNet.Arc;
import carisma.check.activity2petrinet.petriNet.PetriNet;
import carisma.check.activity2petrinet.petriNet.Place;
import carisma.check.activity2petrinet.petriNet.Transition;
import carisma.modeltype.uml2.UMLHelper;


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
		this.model = TestHelper.loadModel(this.filepath, "activity2petrinetSuccess.uml");
		assertNotNull(this.model);
		PetriNet petriNet = new Convert(this.model).convert();
		assertNotNull(petriNet);
		List<Place> places = petriNet.getPlaces();
		List<String> placeNames = new  ArrayList<>();
		List<Arc> arcs = petriNet.getArcs();
		List<Transition> transitions = petriNet.getTransitions();
		List<String> transitionNames = new ArrayList<>();
		for (Place place : places) {
			placeNames.add(place.getName());
		}
		for (Transition transition : transitions) {
			transitionNames.add(transition.getName());
		}
		assertEquals("Wrong number of places", 4, places.size());
		for (ControlNode controlNodes : UMLHelper.getAllElementsOfType(this.model, ControlNode.class)) {
			assertTrue(placeNames.contains(controlNodes.getName()));
		}
		assertEquals("Wrong number of arcs", 8, arcs.size());
		assertEquals("Wrong number of transitions", 4, transitions.size());
		for (OpaqueAction opaqueAction : UMLHelper.getAllElementsOfType(this.model, OpaqueAction.class)) {
			assertTrue(transitionNames.contains(opaqueAction.getName()));
		}
	}
	
	/**
	 * this test tests if the plugin works correct with an empty model.
	 */
	@Test
	public final void testEmptyModel() {
		this.model = TestHelper.loadModel(this.filepath, "activity2petrinetEmptyModel.uml");
		assertNotNull(this.model);
		PetriNet petriNet = new Convert(this.model).convert();
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
		this.model = TestHelper.loadModel(this.filepath, "activity2petrinetNoActivityDiagram.uml");
		assertNotNull(this.model);
		PetriNet petriNet = new Convert(this.model).convert();
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
		TestHelper.unloadModel(this.model);
		this.model = null;
	}

}
