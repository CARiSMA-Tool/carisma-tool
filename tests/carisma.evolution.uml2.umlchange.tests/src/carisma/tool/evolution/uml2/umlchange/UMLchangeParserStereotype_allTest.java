package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;

import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.evolution.AddElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.tests.modelutils.uml.TestHelper;

public class UMLchangeParserStereotype_allTest {

	private String testmodeldir = "resources/models/add-all";

	private Model model = null;
	
	@Test
	@Ignore
	public void regularAddAllTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "RegularAdd_AllModel.uml");
		UMLchangeParser parser = new UMLchangeParser(this.model);

		assertEquals(2, parser.generateDeltaDescriptions().get(0)
				.getAlternatives().get(0).getDeltaElements().size());
		AddElement add1 = (AddElement) parser.generateDeltaDescriptions()
				.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		NamedElement target1 = (NamedElement) add1.getTarget();
		assertEquals("Delete1", target1.getName());
		AddElement add2 = (AddElement) parser.generateDeltaDescriptions()
				.get(0).getAlternatives().get(0).getDeltaElements().get(1);
		NamedElement target2 = (NamedElement) add2.getTarget();
		assertEquals("Delete2", target2.getName());
	}

	@Test
	public void regularSubstAllTest() {
		// model = TestHelper.loadModel(testmodeldir, "RegularSubst_AllModel.uml");
		// UMLchangeParser parser = new UMLchangeParser(model);
		// List<DeltaElement> deltaElements =
		// parser.generateDeltaDescriptions().get(0).getAlternatives().get(0).getDeltaElements();
		// assertEquals(1, deltaElements.size());
		// if(deltaElements.get(0) instanceof AddElement) {
		// assertEquals(1, ((AddElement)(deltaElements.get(0))).getContent());
		// } else {
		// assertTrue(deltaElements.get(0) instanceof DelElement);
		// }
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
