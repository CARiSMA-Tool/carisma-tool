package carisma.tool.evolution.uml2.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.uml2.uml.Model;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.io.ModelImporter;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;



/** ImporterTest.
 * @author bberghoff
 *
 */

public class ImporterTest {
	/**
	 * Path to xml-file resources.
	 */
	private String xmldir = "./resources/xmlfiles";
	/**
	 * Name of the valid model.
	 */
	private static final String VALID_MODEL = "resources/models/io/ImporterDeltaTest.uml";
	
	/** ModelLoder for UML models.
	 */
	private ResourceSet rs = new ResourceSetImpl();
	/** 
	 * Resource of the uml model.
	 */
	private Resource modelres;
	/**
	 * Constant String for an often used name of a Change.
	 */
	private static final String SOME_CHANGE = "someChange(1)";
	
	/**
	 * loads the given model.
	 * @param testmodelname - the model to load
	 */
	public final void loadModel(final String path) throws IOException {
		File testmodelfile = new File(path);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}
	
	/** Test if an AddElement which has a '@null' value in 'values' is correctly transformed to an Entry<String, null>.
	 * @throws IOException 
	 * 
	 */
	@Test
	@Ignore
    public final void getSingleAddNullValueInValues() throws IOException {
	    loadModel(VALID_MODEL);
        ModelImporter importer = new ModelImporter();
        File inputXML = new File(this.xmldir + File.separator + "SingleAddDeltaNullValueInValues.xml");
        assertTrue(inputXML.exists());
        Delta delta = importer.getDelta(inputXML, this.modelres);
        assertNotNull(delta);
        assertFalse(delta.getContent().isEmpty());
	    assertTrue(delta.getAllAdditions().get(0).getValues().get("name") == null);
	}
	
	@Test
	@Ignore
	public void getDeltaSingleAddRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "regNamedEleSingleAddDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllAdditions().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	@Test
	@Ignore
	public void getDeltaSingleDelRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "regNamedEleSingleDelDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllDeletions().size());

		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	@Test
	@Ignore
	public void getDeltaSingleEditRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "regNamedEleSingleEditDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllEdits().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}	
	
	@Test
	@Ignore
	public void getDeltaSingleSubstRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "regNamedEleSingleSubstDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllSubstitutions().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	@Test
	@Ignore
	public void getDeltaSingleStereoAddRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "StereotypeSingleAddDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllAdditions().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	@Test
	@Ignore
	public void getDeltaSingleStereoDelRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		try {
			assertNotNull("Initialized gibts gar nicht!", UMLHelper.getElementByName((Model) this.modelres.getContents().get(0), "INITIALIZED"));
			ModelImporter importer = new ModelImporter();
			File inputXML = new File(this.xmldir + File.separator + "StereotypeSingleDelDeltaWrite.xml");
			assertTrue(inputXML.exists());
			Delta delta = importer.getDelta(inputXML, this.modelres);
			assertNotNull(delta);
			assertFalse(delta.getContent().isEmpty());
		
			assertEquals(1, delta.getAllDeletions().size());
			
			assertEquals(1, delta.getChangeIDs().size());
			assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	@Test
	@Ignore
	public void getDeltaSingleStereoEditRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "StereotypeSingleEditDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllEdits().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}	
	
	@Test
	@Ignore
	public void getDeltaSingleStereoSubstRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "StereotypeSingleSubstDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllSubstitutions().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	
	@Test
	@Ignore
	public void getDeltaTwoStereoAddRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "NamedEleTwoAddDelta.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(2, delta.getAllAdditions().size());
		
		assertEquals(2, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
		assertEquals("anotherChange(2)", delta.getChangeIDs().get(1));
	}
	
	@Test
	@Ignore
	public void getDeltaThreeStereoAddRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "NamedEleThreeAddDelta.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(3, delta.getAllAdditions().size());
			
		assertEquals(2, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
		assertEquals("anotherChange(1)", delta.getChangeIDs().get(1));
	}
	
	@Test
	@Ignore
	public void getDeltaFourStereoAddRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "NamedEleFourAddDelta.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(4, delta.getAllAdditions().size());
		
		assertEquals(3, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
		assertEquals("anotherChange(1)", delta.getChangeIDs().get(1));
		assertEquals("oneMoreChange", delta.getChangeIDs().get(2));
	}
	
	
	@Test
	@Ignore
	public void getDeltaRegularTest() throws IOException {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "regDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllAdditions().size());
		assertEquals(1, delta.getAllDeletions().size());
		assertEquals(1, delta.getAllSubstitutions().size());
		assertEquals(1, delta.getAllEdits().size());
		assertEquals(4, delta.getContent().size());
		
		

		XMLResource xmlRes = (XMLResource) this.modelres;
		boolean hasEdit = false;
		boolean[] allTrue = {false, false, false};
		for (DeltaElement dEle : delta.getContent()) {
			if (dEle instanceof SubstElement) { 
				assertEquals("_5VssQGINEeGeQ6PWdg4u1A", xmlRes.getID(((TaggedValue) dEle.getTarget()).getCorrespondingApplication().getExtendedElement()));
				assertEquals("id", ((TaggedValue) dEle.getTarget()).getName());
				Map<String, Object> values = ((SubstElement) dEle).getComponents().get(0).getValues();
				assertEquals(2, values.size());
				assertEquals("Property", ((SubstElement) dEle).getComponents().get(0).getMetaClass().getName());
				assertEquals("xyz", values.get("value"));
				allTrue[0] = true;
			} else if (dEle instanceof DelElement) { 
				assertEquals("__wzHcGINEeGeQ6PWdg4u1A", 
						xmlRes.getID(((StereotypeApplication) dEle.getTarget()).getExtendedElement()));
				assertEquals("authorized-status", ((StereotypeApplication) dEle.getTarget()).getAppliedStereotype().getName());
				allTrue[1] = true;
			} else if (dEle instanceof AddElement) {
				assertEquals("_oSsTsGIMEeGeQ6PWdg4u1A", xmlRes.getID((dEle.getTarget())));
				
				AddElement content = (AddElement) dEle;
				assertEquals("State", content.getMetaClass().getName());
				assertEquals("someThing", content.getValues().get("name"));
				allTrue[2] = true;
			} else if (dEle instanceof EditElement) { 
				assertEquals("_7DbZYGINEeGeQ6PWdg4u1A", xmlRes.getID((dEle.getTarget())));
				hasEdit = true;
			}
		}
		assertTrue(allTrue[0]);
		assertTrue(allTrue[1]);
		assertTrue(allTrue[2]);
		assertTrue(hasEdit);
		
		assertEquals(3, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
		assertEquals("anotherChange(1)", delta.getChangeIDs().get(1));
		assertEquals("oneMoreChange", delta.getChangeIDs().get(2));
	}
	
	@SuppressWarnings("static-method")
	@Test
	public void getDeltaNullTest() { 
		ModelImporter importer = new ModelImporter();
		assertEquals(null, importer.getDelta(null, null));
	}
	
	/** XML file and UML Model do not match at all.
	 * @throws IOException 
	 * 
	 */
	@Test
	@Ignore
	public final void getDeltaWrongModel() throws IOException {
		loadModel("resources/models/io/ImporterDeltaWrongModel.uml");
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "regDeltaWrite.xml");
		assertEquals(0, importer.getDelta(inputXML, this.modelres).getContent().size());
		
	}
	
	
	@Test
	public void getDeltaMalformedXML() throws IOException { 
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "MalformedDelta.xml");
		assertEquals(null, importer.getDelta(inputXML, this.modelres));
	}
	
	@Test
	@Ignore
	public void getDeltaMalformedXML2() throws IOException { 
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(this.xmldir + File.separator + "MalformedDelta2.xml");
		assertNotNull(inputXML);
		
		Delta delta = importer.getDelta(inputXML, this.modelres);
		assertFalse(delta.getContent().isEmpty());
		
		XMLResource xmlRes = (XMLResource) this.modelres;
		boolean[] allTrue = {false, false};
		for (DeltaElement dEle : delta.getContent()) {
			if (dEle instanceof SubstElement) { 
				assertEquals("_5VssQGINEeGeQ6PWdg4u1A", xmlRes.getID(((TaggedValue) dEle.getTarget()).getCorrespondingApplication().getExtendedElement()));
				assertTrue(((TaggedValue) dEle.getTarget()).getName().equals("id"));
				Map<String, Object> values = ((SubstElement) dEle).getComponents().get(0).getValues();
				assertTrue(((SubstElement) dEle).getComponents().get(0).getMetaClass().getName().equals("Property"));
				assertTrue(values.get("value").equals("xyz"));
				allTrue[0] = true;
			} else if (dEle instanceof AddElement) { 
				assertTrue(
						xmlRes.getID((dEle.getTarget()))
						.equals("_5VssQGINEeGeQ6PWdg4u1A"));
				
				AddElement content = (AddElement) dEle;
				assertTrue(
						content.getMetaClass().getName()
						.equals("State"));
				assertTrue(
						content.getValues().get("name").equals("someThing"));
				allTrue[1] = true;
			}
		}
		assertTrue(allTrue[0] && allTrue[1]);
	}
	
	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
