package carisma.xutils.regulatory.webgui.tests;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Set;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.internal.runners.statements.Fail;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;

import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.webgui.WebOntologyHelper;

/**
 * This class is a test class for the web ontology helper.
 * @author dbuerger
 *
 */
public class WebOntologyHelperTest {
	
	private static WebOntologyHelper woh = null;
	private static RegulatoryOntologyHelper roh = null;


	@BeforeClass
	public static void setClassUp() throws Exception {
		woh = new WebOntologyHelper("resources" + File.separator + "Entire_Law_Ontology.owl");
		roh = woh.getRoh();
	}
	
	/**
	 * Test for createAppropriateElement().
	 */
	@Test
	public final void testCreateAppropriateElement() {
		OWLNamedIndividual individual = woh.getIndividualByName("B_1.16");
		Set<OWLAxiom> axioms = individual.getReferencingAxioms(woh.getOntology());
		int sizeBeforCAE = axioms.size();
		axioms.clear();
			// create the individuals
		woh.createAppropriateElement(individual, "Activity", "test", "testType", "2", "5");
		woh.createAppropriateElement(individual, "Artifact", "test", "testType", "2", "5");
		woh.createAppropriateElement(individual, "Role", "test", "testType", "2", "5");
		woh.createAppropriateElement(individual, "Process", "test", "testType", "2", "5");
		woh.createAppropriateElement(individual, "Property", "test", "testType", "2", "5");
			// test if the number of individuals was created
		axioms = individual.getReferencingAxioms(woh.getOntology());
		assertEquals(sizeBeforCAE + 5, axioms.size());
	}

	/**
	 * Test for getIndividualByName().
	 */
	@Test
	public final void testGetIndividualByName() {
		OWLNamedIndividual bsi1 = roh.createBSIElement("myId1", "myName", "myText");
		OWLNamedIndividual bind1 = roh.createMARiskBindingEntry("myId2", "10", "myText");
		OWLNamedIndividual com1 = roh.createMARiskCommentEntry("myId3", "3", "myText");
		
		assertEquals(bsi1, woh.getIndividualByName("myId1"));
		assertEquals(bind1, woh.getIndividualByName("myId2"));
		assertEquals(com1, woh.getIndividualByName("myId3"));
	}

	/**
	 * Test for getText().
	 */
	@Test
	public final void testGetText() {
		roh.createBSIElement("myId1", "myName", "mein erster schoener Text");
		roh.createMARiskBindingEntry("myId2", "10", "mein zweiter schoener Text");
		roh.createMARiskCommentEntry("myId3", "3", "blubb");
			// test if the text is equal
		assertEquals("mein erster schoener Text", woh.getText("myId1", RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT));
		assertEquals("mein zweiter schoener Text", woh.getText("myId2", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT));
		assertEquals("blubb", woh.getText("myId3", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT));
	}

	/**
	 * Test for getIndividuals().
	 */
	@Test
	public final void testGetIndividuals() {
		Set<OWLNamedIndividual> individuals = woh.getIndividuals(RegulatoryOntologyHelper.CLS_LAW);
		assertTrue(individuals.size() == 2);
	}

	/**
	 * Test for getSubElementsOfRe().
	 */
	@Test
	public final void testGetSubElementsOfRe() {
		assertTrue(woh.getSubElementsOfRe("Activity", "").length() > 1);
		assertTrue(woh.getSubElementsOfRe("Process", "").length() > 1);
		assertTrue(woh.getSubElementsOfRe("Artifact", "").length() > 1);
		assertTrue(woh.getSubElementsOfRe("Role", "").length() > 1);
		assertTrue(woh.getSubElementsOfRe("Property", "").length() > 1);
		assertTrue(woh.getSubElementsOfRe("bla", "").length() == 0);
	}

	/**
	 * Test for getOntology.
	 */
	@Test
	public final void testGetOntology() {	// TODO besserer Test??
		OWLOntology ontology = woh.getOntology();
		assertEquals(ontology, roh.getOntology());
	}

	/**
	 * Test for getReferences().
	 */
	@Test
	public final void testGetReferences() {
		OWLNamedIndividual binding = woh.getIndividualByName("B_1.16");
		OWLNamedIndividual test1 = roh.createBSIMeasure("myId7", "bla", "blubb");
		OWLNamedIndividual test2 = roh.createBSIThreat("myId8", "bla", "blubb");
		roh.assignMeasure(binding, test1);
		roh.assignThreat(binding, test2);
		String newReferences = woh.getReferences("B_1.16");
		assertTrue(newReferences.contains("myId7"));
		assertTrue(newReferences.contains("myId8"));
	}

	/**
	 * Test for getType().
	 */
	@Test
	public final void testGetType() {
		assertEquals("BSIElement", woh.getTypeOfIndividual("B_1.16"));
		assertEquals("Section", woh.getTypeOfIndividual("BDSG-1-1"));
		assertEquals("BSIThreat", woh.getTypeOfIndividual("G_1.1"));
		assertEquals("BSIMeasure", woh.getTypeOfIndividual("M_1.1"));
		assertEquals("MARiskBinding", woh.getTypeOfIndividual("MARiskVA_10_B1"));
		assertEquals("MARiskComment", woh.getTypeOfIndividual("MARiskVA_10_C1"));
	}

	/**
	 * Test for deleteElement().
	 */
	@Test
	public final void testDeleteElement() {
		OWLNamedIndividual individual = woh.getIndividualByName("B_1.16");
			// create the individuals
		woh.createAppropriateElement(individual, "Activity", "test", "Type", "2", "5");
		woh.createAppropriateElement(individual, "Artifact", "test", "Type", "2", "5");
		woh.createAppropriateElement(individual, "Role", "test", "Type", "2", "5");
		woh.createAppropriateElement(individual, "Process", "test", "Type", "2", "5");
		woh.createAppropriateElement(individual, "Process", "tba", "Type", "2", "5");
			// delete element 
		assertEquals("succes", woh.deleteElement("B_1.16", "test", "2", "5"));
		assertEquals("error", woh.deleteElement("B_1.16", "tba", "0", "400"));
	}
	
	
	/**
	 * Test for createRuleElementRelation();
	 */
	@Test
	public final void testCreateRuleElementRelation() {
		woh.getRoh().createREActivity("Boing");
		woh.getRoh().createREArtifact("Brrr");
		woh.getRoh().createRERole("Rol");
		woh.getRoh().createREProcess("Blurk");
		String[] relatedRuleelements = new String[]{"Artifact_Brrr&Activity_Boing", "Role_Rol&Process_Blurk"};
		OWLNamedIndividual boing = woh.getRoh().getGoh().getIndividualByIdAndClass("Activity_Boing", RegulatoryOntologyHelper.CLS_ACTIVITY, true);
		OWLNamedIndividual blurk = woh.getRoh().getGoh().getIndividualByIdAndClass("Process_Blurk", RegulatoryOntologyHelper.CLS_PROCESS, true);
		
			// TODO warum wird nicht "null" zurückgegeben obwohl die Relation nicht existiert?
//		assertNull(woh.getRoh().getGoh().getRelatedIndividuals(boing, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS));
//		assertNull(woh.getRoh().getGoh().getRelatedIndividuals(blurk, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS));
	
		woh.createRuleElementRelation(relatedRuleelements);
			
		assertNotNull(woh.getRoh().getGoh().getRelatedIndividuals(boing, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS));
		assertNotNull(woh.getRoh().getGoh().getRelatedIndividuals(blurk, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS));
	}

}
