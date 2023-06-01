package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Property;
import org.junit.After;
import org.junit.Ignore;
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
	@Ignore
	public final void testMoveProperty() {
		this.model = TestHelper.loadModel(this.filepath, "testMoveProperty.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		Element elementToChange = TestHelper.checkedGetElement(this.model, "Class2", Class.class);
		Element targetElement = TestHelper.checkedGetElement(this.model, "propertyToMove", Property.class);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		EditElement editElement = (EditElement) changeList.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertNotNull(editElement);
		assertEquals(changeList.get(0).getRef(), "toClass2");
		assertEquals(editElement.getValues().get("name"), "moved");
		assertEquals((editElement.getValues().get("owner")), elementToChange);
		assertEquals((editElement.getTarget()), targetElement);
	}
	
	
	/**
	 * tests if no Change will be created if the new target won't exists.
	 */
	@Test
	@Ignore
	public final void testMoveNotExistingTarget() {
		this.model = TestHelper.loadModel(this.filepath, "testMoveNotExistingTarget.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(0, changeList.size());
	}
	
	/**
	 * tests if no Change will be created if the Name of the target Element is not qualified enough.
	 */
	@Test
	@Ignore
	public final void testMoveNameNotQualifiedEnough() {
		this.model = TestHelper.loadModel(this.filepath, "testMoveNameNotQualifiedEnough.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(0, changeList.size());
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
