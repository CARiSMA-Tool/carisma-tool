package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Property;
import org.junit.After;
import org.junit.Test;

import carisma.evolution.Change;
import carisma.evolution.EditElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.tests.modelutils.uml.TestHelper;

/**
 * tests for the move stereotype.
 * @author Klaus Rudack
 *
 */
public class MoveTest {

	/**
	 * the path of the directory where the test models lay in.
	 */
	private String filepath = "resources/models/move";
		
	/**
	 * the original model.
	 */
	private Model model = null;
	
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser;
		
	/**
	 * tests if a correct EditElement will be created to move a property from one class to an other class.
	 */
	@Test
	public final void testMoveProperty() {
		model = TestHelper.loadModel(filepath, "testMoveProperty.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		Element elementToChange = TestHelper.checkedGetElement(model, "Class2", Class.class);
		Element targetElement = TestHelper.checkedGetElement(model, "propertyToMove", Property.class);
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		EditElement editElement = (EditElement) changeList.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertNotNull(editElement);
		assertEquals(changeList.get(0).getRef(), "toClass2");
		assertEquals(editElement.getValues().get("name"), "moved");
		assertEquals(((NamedElement) editElement.getValues().get("owner")), elementToChange);
		assertEquals(((NamedElement) editElement.getTarget()), targetElement);
	}
	
	
	/**
	 * tests if no Change will be created if the new target won't exists.
	 */
	@Test
	public final void testMoveNotExistingTarget() {
		model = TestHelper.loadModel(filepath, "testMoveNotExistingTarget.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertEquals(0, changeList.size());
	}
	
	/**
	 * tests if no Change will be created if the Name of the target Element is not qualified enough.
	 */
	@Test
	public final void testMoveNameNotQualifiedEnough() {
		model = TestHelper.loadModel(filepath, "testMoveNameNotQualifiedEnough.uml");
		parser = new UMLchangeParser(model);
		assertNotNull(parser);
		List<Change> changeList = parser.generateDeltaDescriptions();
		assertEquals(0, changeList.size());
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
