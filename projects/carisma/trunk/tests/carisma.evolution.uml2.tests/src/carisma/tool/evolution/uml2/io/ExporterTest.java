package carisma.tool.evolution.uml2.io;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Pseudostate;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.CopyElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.io.ModelExporter;
import carisma.evolution.uml2.io.datatype.ExportAddElement;
import carisma.evolution.uml2.io.datatype.ExportCopyElement;
import carisma.evolution.uml2.io.datatype.ExportDelElement;
import carisma.evolution.uml2.io.datatype.ExportDelta;
import carisma.evolution.uml2.io.datatype.ExportDeltaElement;
import carisma.evolution.uml2.io.datatype.ExportEditElement;
import carisma.evolution.uml2.io.datatype.ExportExtTagStereotype;
import carisma.evolution.uml2.io.datatype.ExportExtTagTaggedValue;
import carisma.evolution.uml2.io.datatype.ExportSubstElement;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;



// TODO compare test cases from the txt with the implemented.
 /** .
 * 
 * @author bberghoff
 *
 */
public class ExporterTest {
	/** 
	 * XMI-ID of the NamedElement used as Target.
	 */
	private static final String NAMED_ELE_XMI = "_oSsTsGIMEeGeQ6PWdg4u1A";
	/**
	 * XMI of State SECURED.
	 */
	private static final String STATE_SECURED_XMI = "_5VssQGINEeGeQ6PWdg4u1A";
	private static final String PSEUDOSTATE1 = "Pseudostate1";
	private static final String STATE = "State";
	private static final String NAME = "name";
	private static final String NEW_NAME = "newName";
	private static final String VALUE = "value";
	private static final String NAME_VALUE = "someThing";
	private static final String VALUE_VALUE = "xyz";
	private static final String PROPERTY = "Property";
	private static final String SECURED = "SECURED";
	private static final String TEST_MODEL = "ExporterTest.uml";
	private static final String USED_CHANGES = "someChange(1)";
	

	/**
	 * Initializing standard Element each time a new model has been loaded.
	 */
	private void init() {
		model = (Model) modelres.getContents().get(0);
		targetNamedElement = getMember(model, PSEUDOSTATE1, Pseudostate.class);
		
		State extendedEle = getMember(model, SECURED, State.class); 
		targetStereoApp = UMLHelper.getStereotypeApplication(extendedEle, "identifiable"); 
		targetTaggedValue = targetStereoApp.getTaggedValue("id");
	}

	/** ModelLoader.
	 */
	private UML2ModelLoader ml = null;

	/**
	 * Model Resource.
	 */
	private Resource modelres = null;
	
	/** 
	 * The Model.
	 */
	private Model model;
	
	private Pseudostate targetNamedElement;
	
	private StereotypeApplication targetStereoApp; 
	private TaggedValue targetTaggedValue;
	
	
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
		init();
	}
	
	
	/** get a Named Element from a model by name and type.
	 * 
	 * @param model Model to get the Element from 
	 * @param name The Name of the Element
	 * @param clazz The Class of the Element.
	 * @return T The 
	 */
	public final <T extends NamedElement> T getMember(Package model, String name, Class<T> clazz) {
		T member = null;
		try {
			member = UMLHelper.getElementOfNameAndType(model, name, clazz);
			assertNotNull(member);
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "Couldn't find Element, \"" + name + "\".", e);
			fail(e.getMessage());		
		}
		return member;
	}
	
	private void testAddElement(ExportDelta expDelta) {
		assertEquals(1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportAddElement);
		assertEquals(1, ((ExportAddElement) expDelta.getContent().get(0)).getValues().size());
		assertEquals(0, ((ExportAddElement) expDelta.getContent().get(0)).getContent().size());
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			ExportAddElement add = (ExportAddElement) expDeltaEle;
			assertEquals(STATE, add.getType());
			assertEquals(NAME_VALUE, add.getValues().get(NAME));

		}
	}
	
	private void testSubstEle(ExportDelta expDelta) {
		assertEquals("Number of DeltaElements", 1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportSubstElement);
		assertEquals("Components List of AddEle", 1, ((ExportSubstElement) expDelta.getContent().get(0)).getComponents().size());
		assertEquals("Values", 2, ((ExportSubstElement) expDelta.getContent().get(0)).getComponents().get(0).getValues().size());
		
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			ExportSubstElement subst = (ExportSubstElement) expDeltaEle;
			
			assertEquals(PROPERTY, subst.getComponents().get(0).getType());
			assertEquals("id", subst.getComponents().get(0).getValues().get(NAME));
			assertEquals(VALUE_VALUE, subst.getComponents().get(0).getValues().get(VALUE));
		}
	}
	
	private void testEditEle(ExportDelta expDelta) {
		assertEquals(1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportEditElement);
		assertEquals(1, ((ExportEditElement) expDelta.getContent().get(0)).getValues().size());
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			ExportEditElement edit = (ExportEditElement) expDeltaEle;
			assertEquals(NEW_NAME, edit.getValues().get(NAME));
		}
	}
	
	private void testNamedEleTarget(ExportDelta expDelta) {
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			
			assertEquals("PseudostateImpl", expDeltaEle.getTarget().getType());
			assertEquals(PSEUDOSTATE1, expDeltaEle.getTarget().getName());
			assertEquals(NAMED_ELE_XMI, expDeltaEle.getTarget().getXmiID());
		}
	}
	
	
	@Test
	public void generateXMLOutputNamedEleAddTest() {
		
		loadModel(TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
	
			AddElement addEle = new AddElement(targetNamedElement, UMLHelper.getMetaClass(STATE), null);
			
			Map<String, Object> newValues = new HashMap<String, Object>();
			newValues.put(NAME, NAME_VALUE);
		 	addEle.replaceValues(newValues);
		 	deltaElements.add(addEle);
			
		 	List<String> usedChanges = new ArrayList<String>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			ModelExporter exporter = new ModelExporter();
			ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
			testAddElement(expDelta);
			testNamedEleTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}

	}
	
    /** Test whether the null value of an entry in 'values' is processed correctly.
     * 
     */
    @Test
    public final void generateXMLOutputAddEleNullValueInValues() {
        try {
            loadModel(TEST_MODEL);
            AddElement addElement = new AddElement(targetNamedElement, UMLHelper.getMetaClass(STATE), null);
            Map<String, Object> values = new HashMap<String, Object>();
            values.put(NAME, null);
            addElement.replaceValues(values);
            ModelExporter exporter = new ModelExporter();
            List<DeltaElement> content = new ArrayList<DeltaElement>();
            content.add(addElement);
            Delta delta = new Delta(content);
            ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
            assertEquals("@null", ((ExportAddElement) expDelta.getContent().get(0)).getValues().get(NAME)); 
        } catch (InvalidMetaclassException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
            fail("Error while creating test-data.");
        }
    }   
    /** Test whether the null value of an entry in 'values' is processed correctly.
     * 
     */
    @Test
    public final void generateXMLOutputEditEleNullValueInValues() {
        loadModel(TEST_MODEL);
        EditElement editElement = new EditElement(targetNamedElement);
        Map<String, Object> values = new HashMap<String, Object>();
        values.put(NAME, null);
        editElement.replaceValues(values);
        ModelExporter exporter = new ModelExporter();
        List<DeltaElement> content = new ArrayList<DeltaElement>();
        content.add(editElement);
        Delta delta = new Delta(content);
        ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
        assertEquals("@null", ((ExportEditElement) expDelta.getContent().get(0)).getValues().get(NAME));
    }
	
	@Test
	public void generateXMLOutputNamedEleDelTest() {
		
		loadModel(TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		

		DelElement delEle = new DelElement(targetNamedElement);
	 	
	 	deltaElements.add(delEle);
		
		
	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		ModelExporter exporter = new ModelExporter();
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
		
		assertEquals(1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportDelElement);
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			ExportDelElement del = (ExportDelElement) expDeltaEle;

			assertEquals("PseudostateImpl", del.getTarget().getType());
			assertEquals(PSEUDOSTATE1, del.getTarget().getName());
			assertEquals(NAMED_ELE_XMI, del.getTarget().getXmiID());
		}
	}
	
	
	/** Exports one Substitution Element.
	 *  The Target is the NamedElement, Pseudostate1
	 *  The SubstElement has a List of size one with components.
	 *  This one AddElement has NO Target and Two Values.
	 */
	
	@Test
	public void generateXMLOutputNamedEleSubstTest() {
		
		loadModel(TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
			
			List<AddElement> components = new ArrayList<AddElement>();
			AddElement one = new AddElement(null, UMLHelper.getMetaClass(PROPERTY), null);
			
			Map<String, Object> newValues = new HashMap<String, Object>();
			newValues.put(NAME, "id");
			newValues.put(VALUE, VALUE_VALUE);
			one.replaceValues(newValues);
			
			components.add(one);
			
			
			SubstElement substEle = new SubstElement(targetNamedElement, components);
		 	deltaElements.add(substEle);
			
			
		 	List<String> usedChanges = new ArrayList<String>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			ModelExporter exporter = new ModelExporter();
			ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
			
			assertEquals(1, expDelta.getContent().size());
			assertTrue(expDelta.getContent().get(0) instanceof ExportSubstElement);
			assertEquals(1, ((ExportSubstElement) expDelta.getContent().get(0)).getComponents().size());
			assertEquals(2, ((ExportSubstElement) expDelta.getContent().get(0)).getComponents().get(0).getValues().size());
			
			
			for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
				ExportSubstElement subst = (ExportSubstElement) expDeltaEle;
				
				assertEquals("PseudostateImpl", subst.getTarget().getType());
				assertEquals(PSEUDOSTATE1, subst.getTarget().getName());
				assertEquals(NAMED_ELE_XMI, subst.getTarget().getXmiID());
				
	
				assertEquals(PROPERTY, subst.getComponents().get(0).getType());
				assertEquals("id", subst.getComponents().get(0).getValues().get(NAME));
				assertEquals(VALUE_VALUE, subst.getComponents().get(0).getValues().get(VALUE));
			}
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	@Test
	public void generateXMLOutputNamedEleCopyTest() {
		
		loadModel(TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		
		CopyElement cp = new CopyElement(targetNamedElement, getMember(model, SECURED, State.class));
		deltaElements.add(cp);
		
		ModelExporter exporter = new ModelExporter();		
		
		
	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			
			assertEquals("StateImpl", ((ExportCopyElement) expDeltaEle).getTargetOwner().getType());
			assertEquals(SECURED, ((ExportCopyElement) expDeltaEle).getTargetOwner().getName());
			assertEquals(STATE_SECURED_XMI, ((ExportCopyElement) expDeltaEle).getTargetOwner().getXmiID());
		}
		
		testNamedEleTarget(expDelta);
	}
	
	
	@Test
	public void generateXMLOutputNamedEleEditTest() {
		
		loadModel(TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		
	 	EditElement editEle = new EditElement(targetNamedElement);
	 	Map<String, Object> newValues = new HashMap<String, Object>();
	 	newValues.put(NAME, NEW_NAME);
	 	editEle.replaceValues(newValues);
	 	deltaElements.add(editEle);
		
		
	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ModelExporter exporter = new ModelExporter();
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
				
		testEditEle(expDelta);
		testNamedEleTarget(expDelta);
	}
	
	
	private void testStereoAppTarget(ExportDelta expDelta) {
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			ExportExtTagStereotype target = (ExportExtTagStereotype) expDeltaEle.getTarget();
			
			assertEquals("StereotypeApplication", target.getType());
			assertEquals("identifiable", target.getName());
			assertEquals(STATE_SECURED_XMI, target.getXmiID());
			assertEquals("ImporterTestModel::ImporterTestPackage::ImporterTestStateMachine::MainRegion::SECURED", target.getExtendedElement());
			assertEquals("UMLsec", target.getProfile());
		}
		
	}
	
	@Test
	public void generateXMLOutputStereoAppAddTest() {
		
		loadModel(TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
			
	
			AddElement addEle = new AddElement(targetStereoApp, UMLHelper.getMetaClass(STATE), null);
			Map<String, Object> newValues = new HashMap<String, Object>();
			newValues.put(NAME, NAME_VALUE);
		 	addEle.replaceValues(newValues);
		 	deltaElements.add(addEle);
			
			
		 	List<String> usedChanges = new ArrayList<String>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			
			ModelExporter exporter = new ModelExporter();
			ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
			
			testAddElement(expDelta);
			testStereoAppTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	@Test
	public void generateXMLOutputStereoAppDelTest() {
		
		loadModel(TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		

		DelElement delEle = new DelElement(targetStereoApp);
	 	
	 	deltaElements.add(delEle);
		

		
	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ModelExporter exporter = new ModelExporter();
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
		
		assertEquals(1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportDelElement);
		

		testStereoAppTarget(expDelta);

	}
	
	
	/** Exports one Substitution Element.
	 *  The Target is the NamedElement, Pseudostate1
	 *  The SubstElement has a List of size one with components.
	 *  This one AddElement has NO Target and Two Values.
	 */
	
	@Test
	public void generateXMLOutputStereoAppSubstTest() {
		
		loadModel(TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
			
			List<AddElement> components = new ArrayList<AddElement>();
			AddElement one = new AddElement(null, UMLHelper.getMetaClass(PROPERTY), null);
			
			Map<String, Object> newValues = new HashMap<String, Object>();
			newValues.put(NAME, "id");
			newValues.put(VALUE, VALUE_VALUE);
			one.replaceValues(newValues);
			
			components.add(one);
			
			SubstElement substEle = new SubstElement(targetStereoApp, components);
		 	deltaElements.add(substEle);
			
	
		 	List<String> usedChanges = new ArrayList<String>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			
			ModelExporter exporter = new ModelExporter();
			ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
			
			testSubstEle(expDelta);
			testStereoAppTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	
	@Test
	public void generateXMLOutputStereoAppCopyTest() {
		
		loadModel(TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		
		CopyElement cp = new CopyElement(targetStereoApp, getMember(model, SECURED, State.class));
		deltaElements.add(cp);
		
		ModelExporter exporter = new ModelExporter();

	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			
			assertEquals("StateImpl", ((ExportCopyElement) expDeltaEle).getTargetOwner().getType());
			assertEquals(SECURED, ((ExportCopyElement) expDeltaEle).getTargetOwner().getName());
			assertEquals(STATE_SECURED_XMI, ((ExportCopyElement) expDeltaEle).getTargetOwner().getXmiID());
		}
		
		testStereoAppTarget(expDelta);
	}
	
	
	@Test
	public void generateXMLOutputStereoAppEditTest() {
		
		loadModel(TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		
	 	EditElement editEle = new EditElement(targetStereoApp);
	 	
	 	Map<String, Object> newValues = new HashMap<String, Object>();
	 	newValues.put(NAME, NEW_NAME);
	 	
	 	editEle.replaceValues(newValues);
	 	deltaElements.add(editEle);
		

	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ModelExporter exporter = new ModelExporter();
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
		
		testEditEle(expDelta);
		testStereoAppTarget(expDelta);
	}
	
	
	private void testTaggedValueTarget(ExportDelta expDelta) {
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
		ExportExtTagTaggedValue target = (ExportExtTagTaggedValue) expDeltaEle.getTarget();
		
		assertEquals("Tagged Value", target.getType());
		assertEquals("id", target.getName());
		assertEquals(STATE_SECURED_XMI, target.getXmiID());
		assertEquals("ImporterTestModel::ImporterTestPackage::ImporterTestStateMachine::MainRegion::SECURED", target.getExtendedElement());
		assertEquals("UMLsec", target.getProfile());
		assertEquals("identifiable", target.getStereotype());
		}
		
	}
	
	@Test
	public void generateXMLOutputTaggedValueAddTest() {
		
		loadModel(TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
	
	
			AddElement addEle = new AddElement(targetTaggedValue, UMLHelper.getMetaClass(STATE), null);
			Map<String, Object> newValues = new HashMap<String, Object>();
			newValues.put(NAME, NAME_VALUE);
		 	addEle.replaceValues(newValues);
		 	deltaElements.add(addEle);
			
	
		 	List<String> usedChanges = new ArrayList<String>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			
			ModelExporter exporter = new ModelExporter();
			ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
			
			testAddElement(expDelta);
			testTaggedValueTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	
	
	@Test
	public void generateXMLOutputTaggedValueCopyTest() {
		
		loadModel(TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		
		CopyElement cp = new CopyElement(targetTaggedValue, getMember(model, SECURED, State.class));
		deltaElements.add(cp);
		
		ModelExporter exporter = new ModelExporter();

	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			
			assertEquals("StateImpl", ((ExportCopyElement) expDeltaEle).getTargetOwner().getType());
			assertEquals(SECURED, ((ExportCopyElement) expDeltaEle).getTargetOwner().getName());
			assertEquals(STATE_SECURED_XMI, ((ExportCopyElement) expDeltaEle).getTargetOwner().getXmiID());
		}
		
		testTaggedValueTarget(expDelta);
	}
	
	
	
	@Test
	public void generateXMLOutputTaggedValueDelTest() {
		
		loadModel(TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		

		DelElement delEle = new DelElement(targetTaggedValue);
	 	
	 	deltaElements.add(delEle);
		

	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ModelExporter exporter = new ModelExporter();
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
		
		assertEquals(1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportDelElement);
		

		testTaggedValueTarget(expDelta);
	}
	
	
	/** Exports one Substitution Element.
	 *  The Target is the NamedElement, Pseudostate1
	 *  The SubstElement has a List of size one with components.
	 *  This one AddElement has NO Target and Two Values.
	 */
	
	@Test
	public void generateXMLOutputTaggedValueSubstTest() {
		
		loadModel(TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
			
			List<AddElement> components = new ArrayList<AddElement>();
			AddElement one = new AddElement(null, UMLHelper.getMetaClass(PROPERTY), null);
			
			Map<String, Object> newValues = new HashMap<String, Object>();
			newValues.put(NAME, "id");
			newValues.put(VALUE, VALUE_VALUE);
			one.replaceValues(newValues);
			
			components.add(one);
			
			SubstElement substEle = new SubstElement(targetTaggedValue, components);
		 	deltaElements.add(substEle);
			
	
		 	List<String> usedChanges = new ArrayList<String>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			
			ModelExporter exporter = new ModelExporter();
			ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
			
			testSubstEle(expDelta);
			testTaggedValueTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	
	@Test
	public void generateXMLOutputTaggedValueEditTest() {
		
		loadModel(TEST_MODEL);
		List<DeltaElement> deltaElements = new ArrayList<DeltaElement>();
		

	 	EditElement editEle = new EditElement(targetTaggedValue);
	 	Map<String, Object> newValues = new HashMap<String, Object>();
	 	newValues.put(NAME, NEW_NAME);
	 	
	 	editEle.replaceValues(newValues);
	 	deltaElements.add(editEle);
		

	 	List<String> usedChanges = new ArrayList<String>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ModelExporter exporter = new ModelExporter();
		ExportDelta expDelta = exporter.generateXMLOutput(delta, modelres);
		
		testEditEle(expDelta);
		testTaggedValueTarget(expDelta);
	}
}
