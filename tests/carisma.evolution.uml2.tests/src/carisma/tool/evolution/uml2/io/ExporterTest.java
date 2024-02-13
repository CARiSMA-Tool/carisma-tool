package carisma.tool.evolution.uml2.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Pseudostate;
import org.eclipse.uml2.uml.State;
import org.junit.After;
import org.junit.Ignore;
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
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;



// TODO compare test cases from the txt with the implemented.
 /** .
 * 
 * @author bberghoff
 *
 */
@Ignore
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
		this.model = (Model) this.modelres.getContents().get(0);
		this.targetNamedElement = getMember(this.model, PSEUDOSTATE1, Pseudostate.class);
		
		State extendedEle = getMember(this.model, SECURED, State.class); 
		this.targetStereoApp = UMLHelper.getStereotypeApplication(extendedEle, "identifiable"); 
		this.targetTaggedValue = this.targetStereoApp.getTaggedValue("id");
	}

	/** ModelLoader.
	 */
	private ResourceSet rs = new ResourceSetImpl();

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
	private String fipath = "resources/models";
	
	
	/**
	 * loads the given model.
	 * @param testmodelname - the model to load
	 */
	public final void loadModel(final String folder, final String testmodelname) throws IOException {
		File testmodelfile = new File(this.fipath + File.separator + folder + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
		init();
	}
	
	/** get a Named Element from a model by name and type.
	 * 
	 * @param model Model to get the Element from 
	 * @param name The Name of the Element
	 * @param clazz The Class of the Element.
	 * @return T The 
	 */
	public final static <T extends NamedElement> T getMember(Package model, String name, Class<T> clazz) {
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
	
	@SuppressWarnings("static-method")
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
	
	@SuppressWarnings("static-method")
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
	
	@SuppressWarnings("static-method")
	private void testEditEle(ExportDelta expDelta) {
		assertEquals(1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportEditElement);
		assertEquals(1, ((ExportEditElement) expDelta.getContent().get(0)).getValues().size());
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			ExportEditElement edit = (ExportEditElement) expDeltaEle;
			assertEquals(NEW_NAME, edit.getValues().get(NAME));
		}
	}
	
	@SuppressWarnings("static-method")
	private void testNamedEleTarget(ExportDelta expDelta) {
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			
			assertEquals("PseudostateImpl", expDeltaEle.getTarget().getType());
			assertEquals(PSEUDOSTATE1, expDeltaEle.getTarget().getName());
			assertEquals(NAMED_ELE_XMI, expDeltaEle.getTarget().getXmiID());
		}
	}
	
	
	@Test
	public void generateXMLOutputNamedEleAddTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<>();
	
			AddElement addEle = new AddElement(this.targetNamedElement, UMLHelper.getMetaClass(STATE), null);
			
			Map<String, Object> newValues = new HashMap<>();
			newValues.put(NAME, NAME_VALUE);
		 	addEle.replaceValues(newValues);
		 	deltaElements.add(addEle);
			
		 	List<String> usedChanges = new ArrayList<>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
			testAddElement(expDelta);
			testNamedEleTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}

	}
	
    /** Test whether the null value of an entry in 'values' is processed correctly.
     * @throws IOException 
     * 
     */
    @Test
    public final void generateXMLOutputAddEleNullValueInValues() throws IOException {
        try {
            loadModel("io", TEST_MODEL);
            AddElement addElement = new AddElement(this.targetNamedElement, UMLHelper.getMetaClass(STATE), null);
            Map<String, Object> values = new HashMap<>();
            values.put(NAME, null);
            addElement.replaceValues(values);
            List<DeltaElement> content = new ArrayList<>();
            content.add(addElement);
            Delta delta = new Delta(content);
            ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
            assertEquals("@null", ((ExportAddElement) expDelta.getContent().get(0)).getValues().get(NAME)); 
        } catch (InvalidMetaclassException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
            fail("Error while creating test-data.");
        }
    }   
    /** Test whether the null value of an entry in 'values' is processed correctly.
     * @throws IOException 
     * 
     */
    @Test
    public final void generateXMLOutputEditEleNullValueInValues() throws IOException {
        loadModel("io", TEST_MODEL);
        EditElement editElement = new EditElement(this.targetNamedElement);
        Map<String, Object> values = new HashMap<>();
        values.put(NAME, null);
        editElement.replaceValues(values);
        List<DeltaElement> content = new ArrayList<>();
        content.add(editElement);
        Delta delta = new Delta(content);
        ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
        assertEquals("@null", ((ExportEditElement) expDelta.getContent().get(0)).getValues().get(NAME));
    }
	
	@Test
	public void generateXMLOutputNamedEleDelTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<>();
		

		DelElement delEle = new DelElement(this.targetNamedElement);
	 	
	 	deltaElements.add(delEle);
		
		
	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
		
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
	 * @throws IOException 
	 */
	
	@Test
	public void generateXMLOutputNamedEleSubstTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<>();
			
			List<AddElement> components = new ArrayList<>();
			AddElement one = new AddElement(null, UMLHelper.getMetaClass(PROPERTY), null);
			
			Map<String, Object> newValues = new HashMap<>();
			newValues.put(NAME, "id");
			newValues.put(VALUE, VALUE_VALUE);
			one.replaceValues(newValues);
			
			components.add(one);
			
			
			SubstElement substEle = new SubstElement(this.targetNamedElement, components);
		 	deltaElements.add(substEle);
			
			
		 	List<String> usedChanges = new ArrayList<>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
			
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
	public void generateXMLOutputNamedEleCopyTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<>();
		
		CopyElement cp = new CopyElement(this.targetNamedElement, getMember(this.model, SECURED, State.class));
		deltaElements.add(cp);		
		
	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			
			assertEquals("StateImpl", ((ExportCopyElement) expDeltaEle).getTargetOwner().getType());
			assertEquals(SECURED, ((ExportCopyElement) expDeltaEle).getTargetOwner().getName());
			assertEquals(STATE_SECURED_XMI, ((ExportCopyElement) expDeltaEle).getTargetOwner().getXmiID());
		}
		
		testNamedEleTarget(expDelta);
	}
	
	
	@Test
	public void generateXMLOutputNamedEleEditTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<>();
		
	 	EditElement editEle = new EditElement(this.targetNamedElement);
	 	Map<String, Object> newValues = new HashMap<>();
	 	newValues.put(NAME, NEW_NAME);
	 	editEle.replaceValues(newValues);
	 	deltaElements.add(editEle);
		
		
	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
				
		testEditEle(expDelta);
		testNamedEleTarget(expDelta);
	}
	
	
	@SuppressWarnings("static-method")
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
	public void generateXMLOutputStereoAppAddTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<>();
			
	
			AddElement addEle = new AddElement(this.targetStereoApp, UMLHelper.getMetaClass(STATE), null);
			Map<String, Object> newValues = new HashMap<>();
			newValues.put(NAME, NAME_VALUE);
		 	addEle.replaceValues(newValues);
		 	deltaElements.add(addEle);
			
			
		 	List<String> usedChanges = new ArrayList<>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			
			ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
			
			testAddElement(expDelta);
			testStereoAppTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	@Test
	public void generateXMLOutputStereoAppDelTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<>();
		

		DelElement delEle = new DelElement(this.targetStereoApp);
	 	
	 	deltaElements.add(delEle);
		

		
	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
		
		assertEquals(1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportDelElement);
		

		testStereoAppTarget(expDelta);

	}
	
	
	/** Exports one Substitution Element.
	 *  The Target is the NamedElement, Pseudostate1
	 *  The SubstElement has a List of size one with components.
	 *  This one AddElement has NO Target and Two Values.
	 * @throws IOException 
	 */
	
	@Test
	public void generateXMLOutputStereoAppSubstTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<>();
			
			List<AddElement> components = new ArrayList<>();
			AddElement one = new AddElement(null, UMLHelper.getMetaClass(PROPERTY), null);
			
			Map<String, Object> newValues = new HashMap<>();
			newValues.put(NAME, "id");
			newValues.put(VALUE, VALUE_VALUE);
			one.replaceValues(newValues);
			
			components.add(one);
			
			SubstElement substEle = new SubstElement(this.targetStereoApp, components);
		 	deltaElements.add(substEle);
			
	
		 	List<String> usedChanges = new ArrayList<>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			
			ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
			
			testSubstEle(expDelta);
			testStereoAppTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	
	@Test
	public void generateXMLOutputStereoAppCopyTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<>();
		
		CopyElement cp = new CopyElement(this.targetStereoApp, getMember(this.model, SECURED, State.class));
		deltaElements.add(cp);

	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			
			assertEquals("StateImpl", ((ExportCopyElement) expDeltaEle).getTargetOwner().getType());
			assertEquals(SECURED, ((ExportCopyElement) expDeltaEle).getTargetOwner().getName());
			assertEquals(STATE_SECURED_XMI, ((ExportCopyElement) expDeltaEle).getTargetOwner().getXmiID());
		}
		
		testStereoAppTarget(expDelta);
	}
	
	
	@Test
	public void generateXMLOutputStereoAppEditTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<>();
		
	 	EditElement editEle = new EditElement(this.targetStereoApp);
	 	
	 	Map<String, Object> newValues = new HashMap<>();
	 	newValues.put(NAME, NEW_NAME);
	 	
	 	editEle.replaceValues(newValues);
	 	deltaElements.add(editEle);
		

	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
		
		testEditEle(expDelta);
		testStereoAppTarget(expDelta);
	}
	
	
	@SuppressWarnings("static-method")
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
	public void generateXMLOutputTaggedValueAddTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<>();
	
	
			AddElement addEle = new AddElement(this.targetTaggedValue, UMLHelper.getMetaClass(STATE), null);
			Map<String, Object> newValues = new HashMap<>();
			newValues.put(NAME, NAME_VALUE);
		 	addEle.replaceValues(newValues);
		 	deltaElements.add(addEle);
			
	
		 	List<String> usedChanges = new ArrayList<>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
			
			testAddElement(expDelta);
			testTaggedValueTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	
	
	@Test
	public void generateXMLOutputTaggedValueCopyTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<>();
		
		CopyElement cp = new CopyElement(this.targetTaggedValue, getMember(this.model, SECURED, State.class));
		deltaElements.add(cp);

	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
		
		for (ExportDeltaElement expDeltaEle : expDelta.getContent()) {
			
			assertEquals("StateImpl", ((ExportCopyElement) expDeltaEle).getTargetOwner().getType());
			assertEquals(SECURED, ((ExportCopyElement) expDeltaEle).getTargetOwner().getName());
			assertEquals(STATE_SECURED_XMI, ((ExportCopyElement) expDeltaEle).getTargetOwner().getXmiID());
		}
		
		testTaggedValueTarget(expDelta);
	}
	
	
	
	@Test
	public void generateXMLOutputTaggedValueDelTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		
		List<DeltaElement> deltaElements = new ArrayList<>();
		

		DelElement delEle = new DelElement(this.targetTaggedValue);
	 	
	 	deltaElements.add(delEle);
		

	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
		
		assertEquals(1, expDelta.getContent().size());
		assertTrue(expDelta.getContent().get(0) instanceof ExportDelElement);
		

		testTaggedValueTarget(expDelta);
	}
	
	
	/** Exports one Substitution Element.
	 *  The Target is the NamedElement, Pseudostate1
	 *  The SubstElement has a List of size one with components.
	 *  This one AddElement has NO Target and Two Values.
	 * @throws IOException 
	 */
	
	@Test
	public void generateXMLOutputTaggedValueSubstTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		try {
			List<DeltaElement> deltaElements = new ArrayList<>();
			
			List<AddElement> components = new ArrayList<>();
			AddElement one = new AddElement(null, UMLHelper.getMetaClass(PROPERTY), null);
			
			Map<String, Object> newValues = new HashMap<>();
			newValues.put(NAME, "id");
			newValues.put(VALUE, VALUE_VALUE);
			one.replaceValues(newValues);
			
			components.add(one);
			
			SubstElement substEle = new SubstElement(this.targetTaggedValue, components);
		 	deltaElements.add(substEle);
			
	
		 	List<String> usedChanges = new ArrayList<>();
		 	usedChanges.add(USED_CHANGES);
			
			Delta delta = new Delta(usedChanges, deltaElements);
			
			ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
			
			testSubstEle(expDelta);
			testTaggedValueTarget(expDelta);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	
	@Test
	public void generateXMLOutputTaggedValueEditTest() throws IOException {
		
		loadModel("io", TEST_MODEL);
		List<DeltaElement> deltaElements = new ArrayList<>();
		

	 	EditElement editEle = new EditElement(this.targetTaggedValue);
	 	Map<String, Object> newValues = new HashMap<>();
	 	newValues.put(NAME, NEW_NAME);
	 	
	 	editEle.replaceValues(newValues);
	 	deltaElements.add(editEle);
		

	 	List<String> usedChanges = new ArrayList<>();
	 	usedChanges.add(USED_CHANGES);
		
		Delta delta = new Delta(usedChanges, deltaElements);
		
		ExportDelta expDelta = ModelExporter.generateXMLOutput(delta, this.modelres);
		
		testEditEle(expDelta);
		testTaggedValueTarget(expDelta);
	}
	
	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
