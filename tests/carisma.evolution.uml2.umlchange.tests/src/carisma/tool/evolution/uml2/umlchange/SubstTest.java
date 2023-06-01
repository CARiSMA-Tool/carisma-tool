package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Package;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.evolution.AddElement;
import carisma.evolution.Change;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;
import carisma.tests.modelutils.uml.TestHelper;

public class SubstTest {

	private String testmodeldir = "resources/models/subst";
	
	private Model model = null;
	
	private UMLchangeParser parser = null;
		
	@Test
	public void testSubstSimpleElement() {
		this.model = TestHelper.loadModel(this.testmodeldir, "subst_SimpleElement.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		Class class1 = TestHelper.checkedGetElement(this.model, "Class1", Class.class);
		assertTrue(UMLchangeUtil.hasStereotype(UMLchange.SUBST, class1));
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		Change substClass = changes.get(0);
		assertEquals(1, substClass.getAlternatives().size());
		assertEquals(1, substClass.getAlternatives().get(0).getDeltaElements().size());
		SubstElement de = (SubstElement) substClass.getAlternatives().get(0).getDeltaElements().get(0);
		assertEquals(class1, de.getTarget());
		assertEquals(0, de.getAccompanyingDeletions().size());
		assertEquals(1, de.getComponents().size());
		AddElement component = de.getComponents().get(0);
		Package pkg = TestHelper.checkedGetElement(this.model, "subst_SimpleElement", Package.class);
		assertEquals(pkg, component.getTarget());
		assertEquals("Class", component.getMetaClass().getName());
		assertNull(component.getParent());
		assertEquals(1, component.getValues().size());
	}

	@Test
	@Ignore
	public void testSubstSimpleWithKeep() {
		this.model = TestHelper.loadModel(this.testmodeldir, "Subst_SimpleWithKeep.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		Class class1 = TestHelper.checkedGetElement(this.model, "Class1", Class.class);
		assertTrue(UMLchangeUtil.hasStereotype(UMLchange.SUBST, class1));
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		Change substClass = changes.get(0);
		assertEquals(1, substClass.getAlternatives().size());
		assertEquals(1, substClass.getAlternatives().get(0).getDeltaElements().size());
		SubstElement de = (SubstElement) substClass.getAlternatives().get(0).getDeltaElements().get(0);
		assertEquals(class1, de.getTarget());
		assertEquals(1, de.getAccompanyingDeletions().size());
		assertEquals(1, de.getComponents().size());
		AddElement component = de.getComponents().get(0);
		Package pkg = TestHelper.checkedGetElement(this.model, "Subst_SimpleWithKeep", Package.class);
		assertEquals(pkg, component.getTarget());
		assertEquals("Class", component.getMetaClass().getName());
		assertNull(component.getParent());
		assertEquals(1, component.getValues().size());
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
