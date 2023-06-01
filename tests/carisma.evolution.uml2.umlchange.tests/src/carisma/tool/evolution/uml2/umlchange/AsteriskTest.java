package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.evolution.Change;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;
import carisma.tests.modelutils.uml.TestHelper;

/**
 * JUnit testclass for the UMLchangeParser method checkPattern(...).
 * @author Klaus Rudack
 *
 */
public class AsteriskTest {

	/**
	 * the path of the directory where the testmodels lay in.
	 */
	private String filepath = "resources/models/other";
	
	/**
	 * the original model.
	 */
	private Model model = null;
	
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser = null;
	
	/**
	 * tests if the model will be edited correctly if all property ending with "test" will be deleted.
	 */
	@Test
	@Ignore
	public final void testEndsWith() {
		this.model = TestHelper.loadModel(this.filepath, "testStarString.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(2, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
		Element p1 = ((Element) changeList.get(0).getAlternatives().get(0).getDeltaElements().get(0).getTarget()).getOwner();
		assertEquals("Class1", ((NamedElement) p1).getName());
		Element p2 = ((Element) changeList.get(0).getAlternatives().get(0).getDeltaElements().get(1).getTarget()).getOwner();
		assertEquals("Class3", ((NamedElement) p2).getName());
	}
	
	/**
	 * tests if the model will be edited correctly if all property beginning with "test" will be deleted.
	 */
	@Test
	@Ignore
	public final void testBeginWith() {
		this.model = TestHelper.loadModel(this.filepath, "testStarString.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		NamedElement pkg = TestHelper.checkedGetElement(this.model, "testStarStringPkg", NamedElement.class);
		StereotypeApplication delAllApplication = UMLchangeUtil.getStereotypeApplications(pkg).get(0);
		assertNotNull(delAllApplication);
		TaggedValue pattern = delAllApplication.getTaggedValue("pattern");
		assertNotNull(pattern);
		pattern.removeValue();
		pattern.setValue("someRef={Property(name=test*)}");
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(2, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
	}
	
	/**
	 * test if the model will be edited correctly if all property beginning contains "test".
	 */
	@Test
	@Ignore
	public final void testContaining() {
		this.model = TestHelper.loadModel(this.filepath, "testStarString.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		NamedElement pkg = TestHelper.checkedGetElement(this.model, "testStarStringPkg", NamedElement.class);
		StereotypeApplication delAllApplication = UMLchangeUtil.getStereotypeApplications(pkg).get(0);
		assertNotNull(delAllApplication);
		TaggedValue pattern = delAllApplication.getTaggedValue("pattern");
		assertNotNull(pattern);
		pattern.removeValue();
		pattern.setValue("someRef={Property(name=*test*)}");
		TaggedValue p2 = delAllApplication.getTaggedValue("pattern");
		assertEquals("someRef={Property(name=*test*)}", p2.getStringValues().get(0));
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		assertEquals(1, changeList.get(0).getAlternatives().size());
		assertEquals(4, changeList.get(0).getAlternatives().get(0).getDeltaElements().size());
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
