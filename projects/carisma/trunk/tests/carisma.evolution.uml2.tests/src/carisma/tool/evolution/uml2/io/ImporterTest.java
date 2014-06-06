package carisma.tool.evolution.uml2.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.xmi.XMLResource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
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
import carisma.modeltype.uml2.UML2ModelLoader;
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
	private static final String VALID_MODEL = "ImporterDeltaTest.uml";
	
	/** ModelLoder for UML models.
	 */
	private UML2ModelLoader ml = null;
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
	private void loadModel(final String testmodelname) {
		String testmodeldir = "resources/models/io";
		File testmodelfile = new File(testmodeldir + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		if (ml == null) {
			ml = new UML2ModelLoader();
		}
		try {
			modelres = ml.load(testmodelfile);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, "Couldn't load model.", e);
			fail("Couldn't load model");
		}
	}
	
	/** Test if an AddElement which has a '@null' value in 'values' is correctly transformed to an Entry<String, null>.
	 * 
	 */
	@Test
    public final void getSingleAddNullValueInValues() {
	    loadModel(VALID_MODEL);
        ModelImporter importer = new ModelImporter();
        File inputXML = new File(xmldir + File.separator + "SingleAddDeltaNullValueInValues.xml");
        assertTrue(inputXML.exists());
        Delta delta = importer.getDelta(inputXML, modelres);
        assertNotNull(delta);
        assertFalse(delta.getContent().isEmpty());
	    assertTrue(delta.getAllAdditions().get(0).getValues().get("name") == null);
	}
	
	@Test
	public void getDeltaSingleAddRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "regNamedEleSingleAddDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllAdditions().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	@Test
	public void getDeltaSingleDelRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "regNamedEleSingleDelDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllDeletions().size());

		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	@Test
	public void getDeltaSingleEditRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "regNamedEleSingleEditDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllEdits().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}	
	
	@Test
	public void getDeltaSingleSubstRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "regNamedEleSingleSubstDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllSubstitutions().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	@Test
	public void getDeltaSingleStereoAddRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "StereotypeSingleAddDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllAdditions().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	@Test
	public void getDeltaSingleStereoDelRegularTest() {
		loadModel(VALID_MODEL);
		try {
			assertNotNull("Initialized gibts gar nicht!", UMLHelper.getElementByName((Model) modelres.getContents().get(0), "INITIALIZED"));
			ModelImporter importer = new ModelImporter();
			File inputXML = new File(xmldir + File.separator + "StereotypeSingleDelDeltaWrite.xml");
			assertTrue(inputXML.exists());
			Delta delta = importer.getDelta(inputXML, modelres);
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
	public void getDeltaSingleStereoEditRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "StereotypeSingleEditDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllEdits().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}	
	
	@Test
	public void getDeltaSingleStereoSubstRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "StereotypeSingleSubstDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllSubstitutions().size());
		
		assertEquals(1, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
	}
	
	
	@Test
	public void getDeltaTwoStereoAddRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "NamedEleTwoAddDelta.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(2, delta.getAllAdditions().size());
		
		assertEquals(2, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
		assertEquals("anotherChange(2)", delta.getChangeIDs().get(1));
	}
	
	@Test
	public void getDeltaThreeStereoAddRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "NamedEleThreeAddDelta.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(3, delta.getAllAdditions().size());
			
		assertEquals(2, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
		assertEquals("anotherChange(1)", delta.getChangeIDs().get(1));
	}
	
	@Test
	public void getDeltaFourStereoAddRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "NamedEleFourAddDelta.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(4, delta.getAllAdditions().size());
		
		assertEquals(3, delta.getChangeIDs().size());
		assertEquals(SOME_CHANGE, delta.getChangeIDs().get(0));
		assertEquals("anotherChange(1)", delta.getChangeIDs().get(1));
		assertEquals("oneMoreChange", delta.getChangeIDs().get(2));
	}
	
	
	@Test
	public void getDeltaRegularTest() {
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "regDeltaWrite.xml");
		assertTrue(inputXML.exists());
		Delta delta = importer.getDelta(inputXML, modelres);
		assertNotNull(delta);
		assertFalse(delta.getContent().isEmpty());
	
		assertEquals(1, delta.getAllAdditions().size());
		assertEquals(1, delta.getAllDeletions().size());
		assertEquals(1, delta.getAllSubstitutions().size());
		assertEquals(1, delta.getAllEdits().size());
		assertEquals(4, delta.getContent().size());
		
		

		XMLResource xmlRes = (XMLResource) modelres;
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
				assertEquals("_oSsTsGIMEeGeQ6PWdg4u1A", xmlRes.getID(((NamedElement) dEle.getTarget())));
				
				AddElement content = (AddElement) dEle;
				assertEquals("State", content.getMetaClass().getName());
				assertEquals("someThing", content.getValues().get("name"));
				allTrue[2] = true;
			} else if (dEle instanceof EditElement) { 
				assertEquals("_7DbZYGINEeGeQ6PWdg4u1A", xmlRes.getID(((NamedElement) dEle.getTarget())));
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
	
	@Test
	public void getDeltaNullTest() { 
		ModelImporter importer = new ModelImporter();
		assertEquals(null, importer.getDelta(null, null));
	}
	
	/** XML file and UML Model do not match at all.
	 * 
	 */
	@Test
	public final void getDeltaWrongModel() {
		loadModel("ImporterDeltaWrongModel.uml");
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "regDeltaWrite.xml");
		assertEquals(0, importer.getDelta(inputXML, modelres).getContent().size());
		
	}
	
	
	@Test
	public void getDeltaMalformedXML() { 
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "MalformedDelta.xml");
		assertEquals(null, importer.getDelta(inputXML, modelres));
	}
	
	@Test
	public void getDeltaMalformedXML2() { 
		loadModel(VALID_MODEL);
		ModelImporter importer = new ModelImporter();
		File inputXML = new File(xmldir + File.separator + "MalformedDelta2.xml");
		assertNotNull(inputXML);
		
		Delta delta = importer.getDelta(inputXML, modelres);
		assertFalse(delta.getContent().isEmpty());
		
		XMLResource xmlRes = (XMLResource) modelres;
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
						xmlRes.getID(((NamedElement) dEle.getTarget()))
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
}
