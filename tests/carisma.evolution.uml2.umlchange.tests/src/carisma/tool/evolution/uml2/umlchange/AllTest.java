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

import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.tests.modelutils.uml.TestHelper;


/** Tests the umlChangeParser with special interest to the circumstance that a TaggedValue is a Property.
 * 
 * @author bberghoff
 *
 */
public class AllTest {

	/** 
	 * Path of the testmodels.
	 */
	private String testmodeldir = "resources/models/all";
		
	/**
	 * The test model.
	 */
	private Model model = null;
	
	/** 
	 * UMLchangeParser.
	 */
	private UMLchangeParser parser = null;
		
	/** Package1<< del-all>>, with pattern: delAll={Class(name=Class*, contents=<Stereotype(contents=<Property(name=id)>)>)}.<br>
	 * 	Package1::Class1 << identifiable>><br>
	 * 	Package1::Class2 << critical>>
	 *  <br></br>
	 * 	it is expected that one Change with one Alternative and one DeltaElement is returned 
	 *  and that this DeltaElement has Class1 as Target
	 */
	@Test
	@Ignore
	public final void delSingleClassTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "del-allSingleClass.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());
		assertEquals(1, changes.get(0).getAlternatives().get(0).getDeltaElements().size());
		
		DelElement delEle = (DelElement) changes.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		
		Element extendedEle = TestHelper.checkedGetElement(this.model, "Package1::Class1", Class.class);

		assertEquals(extendedEle, delEle.getTarget());
	}
	
	/** Package Package<< del-all>>, with pattern: delAll={Property()}. <br>
	 *  Package::Class1 with property name: classProperty <br>
	 *  Package::Class2 << identifiable>> 
	 */
	@Test
	@Ignore
	public final void delPropertyTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "DelAllProperties.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());
		assertEquals(1, changes.get(0).getAlternatives().get(0).getDeltaElements().size());
		
		DeltaElement delEle = changes.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		
		Element extendedEle = TestHelper.checkedGetElement(this.model, "classProperty", Property.class);
		
		assertEquals(extendedEle, delEle.getTarget());
	}
	
	/** Package Package1<< del-all>>, with <br> pattern: dall={Stereotype(contents=<TaggedValue()>)}   <br> ext: dall=identifiable. <br></br>
	 *  Package1::Class1 << identifiable>> (with TaggedValue 'id':'') <br>
	 *  Package1::Class1::Operation1 << edit>> (no TaggedValue set)
	 *  <br></br>
	 *  Package1::Class2 << identifiable>> (with TaggedValue 'id':fds)
	 */
	@Test
	@Ignore
	public final void delTaggedValueTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "DelAllTaggedValue.uml"); 
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());
		assertEquals(1, changes.get(0).getAlternatives().get(0).getDeltaElements().size());
		
		DeltaElement delEle = changes.get(0).getAlternatives().get(0).getDeltaElements().get(0);
		
		Element extendedEle = TestHelper.checkedGetElement(this.model, "Class1", Class.class);

		StereotypeApplication stereoApp = UMLHelper.getStereotypeApplication(extendedEle, "identifiable");
		assertEquals(stereoApp.getTaggedValue("id"), delEle.getTarget());
	}
	
	/** Package Package1<< del-all>>, with pattern : delAll={TaggedValue(value=something)}.
	 *  Package1::Class1 << identifiable>> (with TaggedValue 'id':something) <br>
	 *  Package1::Class2 << identifiable>> (with TaggedValue 'id':fds)
	 */
	@Test
	@Ignore
	public final void getPatternMatchesTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "GetPatternMatches.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);		
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());
		Alternative alter1 = changes.get(0).getAlternatives().get(0);
		assertEquals(1, alter1.getDeltaElements().size());
		
		DelElement delEle = (DelElement) alter1.getDeltaElements().get(0);
		Class class1 = TestHelper.checkedGetElement(this.model, "Class1", Class.class);
		assertEquals(UMLHelper.getAppliedStereotype(class1, "identifiable"), ((StereotypeApplication) delEle.getTarget()).getAppliedStereotype());
	}
	
	/** Package1 << del-All>> with three Changes. <br>
	 *  1. pattern : delAll1={Class(contents=<Stereotype(contents=<Property()>)>)} <br>
	 *  	ext:  <br></br>
	 *  2. pattern : delAll2={Stereotype()} <br>
	 *  	ext: delAll2=add <br></br>
	 *  3. pattern : delAll3={TaggedValue()} <br>
	 *  	ext: delAll3=add.new <br>
	 *  </br>
	 *  Package1::Class1 <<add>>    (from the UMLChangeProfile) <br></br>
	 *  
	 *  expected output from the parser are zero Changes.
	 */
	@Test
	@Ignore
	public final void ignoreUMLChangeTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "ignoreUMLChangeStereotypes.uml");
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser);
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(0, changes.size());
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
