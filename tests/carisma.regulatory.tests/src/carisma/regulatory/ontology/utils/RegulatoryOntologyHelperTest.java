package carisma.regulatory.ontology.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.security.acl.Owner;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;

/**
 * JUnit test-cases for RegulatoryOntologyHelper.
 * @author Klaus Rudack
 *
 */
public class RegulatoryOntologyHelperTest {

	/**
	 * GenericOntologyHelper.
	 */
	private GenericOntologyHelper goh = new GenericOntologyHelper();
	
	/**
	 * RegulatoryOntologyHelper.
	 */
	private RegulatoryOntologyHelper roh = null;
	
	/**
	 * OWL process.
	 */
	private OWLNamedIndividual process1 = null;
	
	/**
	 * OWL process.
	 */
	private OWLNamedIndividual process2 = null;
	
	/**
	 * OWL role.
	 */
	private OWLNamedIndividual role1 = null;
	
	/**
	 * OWL role.
	 */
	private OWLNamedIndividual role2 = null;
	
	/**
	 * OWL activity.
	 */
	private OWLNamedIndividual act1 = null;
	
	/**
	 * OWL activity.
	 */
	private OWLNamedIndividual act2 = null;
	
	/**
	 * OWL object.
	 */
	private OWLNamedIndividual obj = null;
	
	/**
	 * OWL property.
	 */
	private OWLNamedIndividual prop = null;
	
	
	
	/**
	 * method to initialize the Ontology.
	 */
    @Before
	public final void initialize() {
    	goh.createNewOntology(RegulatoryOntologyHelper.ONTOLOGY_IRI_BASE);		
		roh = new RegulatoryOntologyHelper(goh);
		roh.initializeOntology(goh);
		process1 = roh.createREProcess("id_klsddflks");
		process2 = roh.createREProcess("id_klsfsdfddflks");
		role1 = roh.createRERole("id_lsdfjlslfs");
		role2 = roh.createRERole("id_lsdfdsdfsjlslfs");
		act1 = roh.createREActivity("id_sdlkjflsj");
		act2 = roh.createREActivity("id_sdlsfskjflsj");
		obj = roh.createREArtifact("id_dksjflsjlfj");
     }
	
	
	/**
	 * this test tests the create-methods and getRelatedIndividuals() method.
	 */
	@Test
	public final void testCreate() {
		OWLNamedIndividual bsi = roh.createBSIElement("bsi", "BSIElement", "Dies ist ein Text");
		assertNotNull(bsi);
		
		OWLNamedIndividual activity = roh.createREActivity("Sichern");		
		assertNotNull(activity);
		
		roh.createContainedRuleElementsRelation(bsi, activity, "Dies ist ein Text", 20, 100);		
		Set<OWLNamedIndividual> relatedIndividuals = goh.getRelatedIndividuals(bsi, RegulatoryOntologyHelper.REL_RULE_RULEELEMENTS);
		assertNotNull(relatedIndividuals);
		
		assertTrue("RuleElement was not correct created", relatedIndividuals.contains(activity));
	}

	
	/**
	 * this test tests the getIndividualByName() method of the GenericOntologyHelper.
	 * this method seems to be incorrect and return the element with the  given string as id
	 * instead id name
	 */
	@Test
	public final void testGetIndividualByName() {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		goh.createNewOntology(RegulatoryOntologyHelper.ONTOLOGY_IRI_BASE);		
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		roh.initializeOntology(goh);
		roh.createRE("id", "type", "name");
		OWLNamedIndividual test = goh.getIndividualById("id"); //<-- get name returns the  id
		assertNotNull(test);
	}
	
	/**
	 * this test tests the getStringAnnotation() method.
	 */
	@Test
	public final void testGetStringAnnotation() {
//		OWLNamedIndividual owlTest = roh.createREProcess("id_String", "type_String", "name_String", "text_String", "1", "3");
//		String resultString = null;
//		try {
//			resultString = goh.getStringAnnotation(owlTest, RegulatoryOntologyHelper.PROP_RULEELEMENT_REPRESENTATION);
//		} catch (NoSuchPropertyException e) {
//			e.printStackTrace();
//			fail("Exception in getStringAnnotation() thrown!");
//		}
//		assertNotNull(resultString);
//		assertEquals("text_String", resultString);
	}
	
	/**
	 * this test tests the getBooleanAnnotation() method.
	 */
	@Test
	public final void testGetBooleanAnnotation() {
		String annotationName = "testAnnotation";
		boolean testValue = true;
		OWLNamedIndividual test = roh.createRE("id_String", "type_String", "name_String");
		goh.createAnnotation(test, annotationName, testValue);
		try {
			assertEquals(testValue, goh.getBooleanAnnotation(test, annotationName));
		} catch (NoSuchPropertyException e) {
			e.printStackTrace();
			fail("Exception in getBooleanAnnotation().");
		}
	}
	
	
//	/**
//	 * this test tests the getFloatAnnotation() method.
//	 */
//	@Test
//	public final void testGetFloatAnnotation() {
//		String annotationName = "testAnnotation";
//		float testValue = (float) 1.05;
//		OWLNamedIndividual test = roh.createRE("id_String", "type_String", "name_String", "text_String");
//		goh.createAnnotation(test, annotationName, testValue);
//		try {
//			assertEquals(testValue, goh.getFloatAnnotation(test, annotationName));
//		} catch (NoSuchPropertyException e) {
//			e.printStackTrace();
//			fail("Exception in getFloatAnnotation().");
//		}
//	}
//	TODO KR: es gibt keine assertEquals fuer floats?
	
	/**
	 * this test tests the getIntegerAnnotation() method.
	 */
	@Test
	public final void testGetIntegerAnnotation() {
		String annotationName = "testAnnotation";
		int testValue = 3;
		OWLNamedIndividual test = roh.createRE("id_String", "type_String", "name_String");
		goh.createAnnotation(test, annotationName, testValue);
		try {
			assertEquals(testValue, goh.getIntegerAnnotation(test, annotationName));
		} catch (NoSuchPropertyException e) {
			e.printStackTrace();
			fail("Exception in getIntegerAnnotation().");
		}
	}
	
	/**
	 * this test tests the removeAnnotation() method.
	 * @throws NoSuchPropertyException 
	 */
	@Test(expected = NoSuchPropertyException.class)
	public final void testRemoveAnnotation() throws NoSuchPropertyException {
		String annotationName = "testAnnotation";
		int testValue = 3;
		OWLNamedIndividual test = roh.createRE("id_String", "type_String", "name_String");
		goh.createAnnotation(test, annotationName, testValue);
		try {
			assertEquals(testValue, goh.getIntegerAnnotation(test, annotationName));
		} catch (NoSuchPropertyException e) {
			e.printStackTrace();
			fail("Exception in getIntegerAnnotation().");
		}
		goh.removeAnnotation(test, annotationName);
		goh.getIntegerAnnotation(test, annotationName);
	}
	
	/**
	 * this test tests the convertToValidOwlId() method.
	 */
	@Test
	public final void testConvertToValidOwlId() {
		String expected  = "Dies_ist_ein_test.";
		String test = RegulatoryOntologyHelper.convertToValidOwlId("Dies ist ein test.");
		assertEquals(expected, test);
	}
	
	/**
	 * this test tests the getIndividuals() method.
	 */
	@Test
	public final void testGetIndividuals() {
		assertEquals(2, goh.getIndividuals(RegulatoryOntologyHelper.CLS_PROCESS, false).size());
		assertEquals(2, goh.getIndividuals(RegulatoryOntologyHelper.CLS_PROCESS, true).size());
//		TODO KR: keine subclassen gefunden, fehlt noch um direct (der boolsche wert) richtig zu testen.
	}
	

	/**
	 * this test tests the getOWLClass() method.
	 */
	@Test
	public final void testGetOWLClass() {
		OWLClass oc = goh.getOWLClass(RegulatoryOntologyHelper.CLS_LAW);
		assertEquals(RegulatoryOntologyHelper.CLS_LAW, oc.getIRI().getFragment());
	}
	
	/**
	 * this test tests the setPrefix() method.
	 */
	@Test
	public final void testSetPrefix() {
		String prefix = "test.prefix/";
		String expected = "<test.prefix/classes/c>";
		goh.setPrefix(prefix);
		String test = goh.getOWLClass("c").toString();
		assertEquals(expected, test);
	}
	
	/**
	 * this test tests the saveOntologyToFile() method.
	 */
	@Test
	public final void testSaveOntologyToFile() {
		//Create ontology and save it
		File file = new File("resources/models/new.owl");
		GenericOntologyHelper goh2 = new GenericOntologyHelper();
		goh2.createNewOntology(RegulatoryOntologyHelper.ONTOLOGY_IRI_BASE);		
		RegulatoryOntologyHelper roh2 = new RegulatoryOntologyHelper(goh2);
		roh2.initializeOntology(goh2);
		process1 = roh2.createBSIElement("bla", "bsi", "Dies ist ein Text");
		
			// Auskommentiert das es so nicht funktioniert. Es werden eigene Ids generiert, 
			// also kann man nicht nach dan uebergebenen abfragen. (db)
//		process2 = roh2.createREProcess("id_klsfsdfddflks");
//		role1 = roh2.createRERole("id_lsdfjlslfs");
//		role2 = roh2.createRERole("id_lsdfdsdfsjlslfs");
//		act1 = roh2.createREActivity("id_sdlkjflsj");
//		act2 = roh2.createREActivity("id_sdlsfskjflsj");
//		obj = roh2.createREArtifact("id_dksjflsjlfj");
//		prop = roh2.createREProperty("id_dlkjflsalf");
		goh2.saveOntologyToFile(file);
		
		//load the saved ontology
		GenericOntologyHelper helper = new GenericOntologyHelper();
		OWLOntology oo = helper.getOntology();
		assertNull(oo);
		helper.loadOWLOntologyFromFile(file);
		oo = helper.getOntology();
		assertNotNull(oo);
		OWLNamedIndividual oni = helper.getIndividualById("bla");
		assertNotNull(oni);
		
		
		//delete the ontology from the filesystem
		if (file.exists()) {
			file.delete();
		}
	}
	
	/**
	 * this test tests the createNewOntology() method.
	 */
	@Test
	public final void testCreateNewOntology() {
		String toFind = "M_4.116";
		OWLNamedIndividual oni = null;
		File file = new File("resources/models/Entire_Law_Ontology.owl");
		GenericOntologyHelper helper = new GenericOntologyHelper();
		helper.loadOWLOntologyFromFile(file);
		OWLOntology oo = helper.getOntology();
		assertNotNull(oo);
		oni = helper.getIndividualById(toFind);
		assertNotNull(oni);
		helper.createNewOntology("test");
		oni = helper.getIndividualById(toFind);
		assertNull(oni);
	}
	
	/**
	 * this test tests the loadOWLOntologyFromFile() method.
	 */
	@Test
	public final void testLoadOWLOntologyFromFile() {
		String toFind = "M_4.116";
		OWLNamedIndividual oni = null;
		File file = new File("resources/models/Entire_Law_Ontology.owl");
		GenericOntologyHelper helper = new GenericOntologyHelper();
		OWLOntology oo = helper.getOntology();
		assertNull(oo);
		helper.loadOWLOntologyFromFile(file);
		oo = helper.getOntology();
		assertNotNull(oo);
		oni = helper.getIndividualById(toFind);
		assertNotNull(oni);
	}
	
	/**
	 * this is the test for getReferredRules().
	 */
	@Test
	public final void testGetReferredRules() {
		OWLNamedIndividual law = roh.createLaw("myId", "myLaw");
		roh.createParagraph(law, "1", "myFirstParagraph");
		roh.createParagraph(law, "2", "mySecondParagraph");
		roh.createParagraph(law, "3", "myThirdParagraph");
		roh.createParagraph(law, "4", "myFourthParagraph");
		
		OWLNamedIndividual bsiElement = roh.createBSIElement("BSIElem1", "1", "text");
		OWLNamedIndividual bsiMeasure = roh.createBSIMeasure("BSIMeas1", "1", "text");
		OWLNamedIndividual bsiThreat = roh.createBSIThreat("BSIthea", "1", "text");
		roh.assignMeasure(bsiElement, bsiMeasure);
		roh.assignThreat(bsiElement, bsiThreat);

		OWLNamedIndividual mariskB1 = roh.createMARiskBindingEntry("myBinding1", "1", "text");
		OWLNamedIndividual mariskC1 = roh.createMARiskCommentEntry("myComment1", "2", "text");
		OWLNamedIndividual mariskC2 = roh.createMARiskCommentEntry("myComment2", "3", "text");
		roh.createMARiskBinding(mariskB1, mariskC1);
		roh.createMARiskBinding(mariskB1, mariskC2);
		if (law == null) {
			fail("create law has failed!");
		} else {
			List<OWLNamedIndividual> individuals = roh.getReferredRules(law);
//			System.out.println(Arrays.toString(individuals.toArray()));
			assertTrue(individuals.size() == 4);
		}
		if (mariskB1 == null) {
			fail("create mariskBinding has failed!");
		} else {
			List<OWLNamedIndividual> individuals = roh.getReferredRules(mariskB1);
//			System.out.println(Arrays.toString(individuals.toArray()));
			assertTrue(individuals.size() == 2);
		}
		if (bsiElement == null) {
			fail("create mariskBinding has failed!");
		} else {
			List<OWLNamedIndividual> individuals = roh.getReferredRules(bsiElement);
//			System.out.println(Arrays.toString(individuals.toArray()));
			assertTrue(individuals.size() == 2);
		}
		
		
	}
	
	/**
	 * this is the test for getSubElementsOfRe().
	 */
	@Test
	public final void testGetAllSubElementsOfRe() {
		OWLOntology ontology = goh.getOntology();
		goh.removeOWLOntology(ontology);
		goh.createNewOntology(RegulatoryOntologyHelper.ONTOLOGY_IRI_BASE);
		roh = new RegulatoryOntologyHelper(goh);
		List<String> subElements = null;
		roh.createREActivity("fgdfsgd");
		roh.createREActivity("fgdgdg");
		roh.createREProcess("dfgrgdg");
		roh.createREProcess("fdfdgfdgd");
		roh.createREArtifact("sdfgrgdg");
		roh.createREArtifact("afdfdgfdgd");
		
		subElements = roh.getAllSubElementsOfRE("Activity");
		System.out.println("Size of subElements: " + subElements);
		assertTrue(subElements.size() == 3); 
		subElements.clear();
		
		subElements = roh.getAllSubElementsOfRE("Process");
		assertTrue(subElements.size() == 3);
		subElements.clear();
		
		subElements = roh.getAllSubElementsOfRE("Artifact");
		assertTrue(subElements.size() == 3);
		subElements.clear();
		
		subElements = roh.getAllSubElementsOfRE("Property");
		assertTrue(subElements.size() == 1); // the default value (e.g. Property)
	}
	
	/**
	 * this is the test for getIndicesOfRe().
	 */
	@Test
	public final void testGetIndicesOfRE() {
		OWLOntology ontology = goh.getOntology();
		goh.removeOWLOntology(ontology);
		goh.loadOWLOntologyFromFile(new File("resources/models/Entire_Law_Ontology.owl"));
		OWLNamedIndividual marisk = goh.getIndividualByIdAndClass("B_1.0", 
																   RegulatoryOntologyHelper.CLS_BSIELEMENT, 
																   true);
		OWLNamedIndividual newActivity = roh.createREActivity("newActivity");
		roh.createContainedRuleElementsRelation(marisk, newActivity, "Dies ist ein Text", 10, 20);
		List<String> indices = roh.getIndicesOfRE("B_1.0");
		for (String str : indices) {
			System.out.println(str);
		}
		
	}
	
	/**
	 * this is the test for getAnnotationFromIndividual().
	 */
	@Test
	public final void testGetAnnotationFromIndividual() {
		OWLNamedIndividual individual = roh.createMARiskBindingEntry("myId", "10", "der sehr informative Text.");
		assertEquals("10", roh.
				getAnnotationFromIndividual(individual, RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER));
		assertEquals("der sehr informative Text.", roh.
				getAnnotationFromIndividual(individual, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT));
		assertEquals("", roh.
				getAnnotationFromIndividual(individual, RegulatoryOntologyHelper.CLS_ACTIVITY));
	}
	
	/**
	 * this test tests getSituationFromRule().
	 */
	@Test
	public final void testGetSituationsFromRule() {
		String sigma = "MARiskVA_7.2.1_B1_SigmaVAP";
		String vap = "MARiskVA_7.2.1_B1_VierAugenPrinzip";
		try{ 
			OWLOntology ontology = goh.getOntology();
			goh.removeOWLOntology(ontology);
			goh.loadOWLOntologyFromFile(new File("resources/models/Entire_Law_Ontology.owl"));
			OWLNamedIndividual marisk_rule = goh.getIndividualByIdAndClass("MARiskVA_7.2.1_B1", 
																			RegulatoryOntologyHelper.CLS_MARISKBINDING, 
																			true);
			assertNotNull(marisk_rule);
			Set<OWLNamedIndividual> situations = roh.getSituationsFromRule(marisk_rule);
			assertTrue(2 == situations.size());
			for (OWLNamedIndividual situation : situations) {
				assertTrue(sigma.equals(situation.getIRI().getFragment())
							|| vap.equals(situation.getIRI().getFragment()));
			}
		} catch (Exception e) {
			e.printStackTrace();
			fail(e.getLocalizedMessage());
		}
		
	}
	
	/**
	 * this test tests the createRelatedRuleElementsRelation.
	 */
	@Test
	public final void testCreateRelatedRuleElementsRelation() {
		OWLNamedIndividual sourceRuleElement = roh.createREActivity("myActivity");
		OWLNamedIndividual targetRuleElement = roh.createREArtifact("myArtifact");
		roh.createRelatedRuleElementsRelation(sourceRuleElement, targetRuleElement, 
				RegulatoryOntologyHelper.REL_ACTIVITY_USEDARTIFACTS, 
				"Dies ist ein schoener Text");
		Set<OWLNamedIndividual> test = goh.getRelatedIndividuals(sourceRuleElement, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
		
		assertTrue(test.size() > 0);
		
		boolean contains = false;
		for (OWLNamedIndividual individual : test) {
			if (individual.getIRI().getFragment().equals(targetRuleElement.getIRI().getFragment())) {
				contains = true;
			}
		}
		assertTrue(contains);
	}

}
