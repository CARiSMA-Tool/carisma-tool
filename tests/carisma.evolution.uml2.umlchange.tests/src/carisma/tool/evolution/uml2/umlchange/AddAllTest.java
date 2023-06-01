package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.ControlFlow;
import org.eclipse.uml2.uml.ExpansionRegion;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.OpaqueAction;
import org.eclipse.uml2.uml.UMLFactory;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.profile.umlchange.UMLchange;
import carisma.profile.umlchange.UMLchangeUtil;
import carisma.tests.modelutils.uml.TestHelper;


/**
 * JUnit testclass for the UMLchange-stereotype add-all.
 * @author Klaus Rudack
 *
 */
public class AddAllTest {

	/**
	 * the path of the directory where the testmodels lay in.
	 */
	private String filepath = "resources/models/add-all";
			
	/**
	 * the original model.
	 */
	private Model model = null;
	
	/**
	 * UMLchangeParser for parsing the model.
	 */
	private UMLchangeParser parser = null;
	
	/**
	 * test.
	 */
	@Test
	@Ignore
	public final void testMain() {
		this.model = TestHelper.loadModel(this.filepath, "add-all.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(1, changeList.size());
		assertEquals(2, changeList.get(0).getAlternatives().size());
		for (Alternative alt : changeList.get(0).getAlternatives()) {
			assertEquals("public",  ((NamedElement) alt.getDeltaElements().get(0).getTarget()).getVisibility().toString());
		}
	}
	
	
	/**
	 * test.
	 */
	@Test
	@Ignore
	public final void testFalseUML2Class() {
		this.model = TestHelper.loadModel(this.filepath, "add-all.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		NamedElement packageElement = TestHelper.checkedGetElement(this.model, "add-allPkg", NamedElement.class);
		StereotypeApplication addAllApplication = UMLchangeUtil.getStereotypeApplication(UMLchange.ADDALL, packageElement);
		assertNotNull(addAllApplication);
		TaggedValue pattern = addAllApplication.getTaggedValue("pattern");
		assertNotNull(pattern);
		pattern.removeValue();
		pattern.setValue("someRef={InvalidUML2Class(visibility=public)}");		
		List<Change> changeList = this.parser.generateDeltaDescriptions();
		assertEquals(0, changeList.size());
	}
	
	/**
	 * test.
	 */
	@Test
	@Ignore
	public final void test() {
		this.model = TestHelper.loadModel(this.filepath, "add-all.uml");
		UMLFactory uml = UMLFactory.eINSTANCE;
		ExpansionRegion e = uml.createExpansionRegion();
		OpaqueAction o = uml.createOpaqueAction();
		ActivityPartition a = uml.createActivityPartition();
		assertEquals(0, a.getNodes().size());
		a.getNodes().add(o);
		assertEquals(1, a.getNodes().size());
		a.getNodes().add(e);
		assertEquals(2, a.getNodes().size());
		ControlFlow f = uml.createControlFlow();
		a.getEdges().add(f);
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
