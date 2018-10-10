package carisma.regulatory.ontology.utils;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.OWLOntologyInputSourceException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.DefaultPrefixManager;

import uk.ac.manchester.cs.owl.owlapi.OWLAnnotationAssertionAxiomImpl;
import uk.ac.manchester.cs.owl.owlapi.OWLAnnotationImpl;

public class GenericOntologyHelperTest {

	private GenericOntologyHelper goh_raw;
	private GenericOntologyHelper goh_init;
	
	private static final String TESTIRI = "http://carisma.umlsec.de/ontologies/regulatory/junit/";
	
//	@BeforeClass
//	public static void setUpBeforeClass() throws Exception {
//	}
//
//	@AfterClass
//	public static void tearDownAfterClass() throws Exception {
//	}

	@Before
	public void setUp() throws Exception {
		this.goh_raw = new GenericOntologyHelper();
		
		this.goh_init = new GenericOntologyHelper();
		this.goh_init.createNewOntology(GenericOntologyHelperTest.TESTIRI);
		this.goh_init.setPrefix(TESTIRI);

	}

	@After
	public void tearDown() throws Exception {
		this.goh_raw = null;
		this.goh_init = null;		
	}

	// Test für Konstruktor
	@Test
	public void testGenericOntologyHelper() {
		// Created in @Before: setUp()
		assertNotNull("Ontologie der Unit-Klasse wurde nicht erzeugt", this.goh_raw);				
		
		GenericOntologyHelper gohLoc = new GenericOntologyHelper();
		assertNotNull("Lokale Ontologie wurde nicht erzeugt", gohLoc);
		
		assertNull("Direkt nach dem Konstruktor sollte noch keine Ontologie gesetzt sein", this.goh_raw.getOntology());		
	}

	@Test
	public void testGetOntology() {
		assertNull("Direkt nach dem Konstruktor sollte noch keine Ontologie gesetzt sein", this.goh_raw.getOntology());
	}


	@Test
	public void testCreateNewOntology() {
		this.goh_raw.createNewOntology(GenericOntologyHelperTest.TESTIRI);
		assertNotNull(this.goh_raw.getOntology());		
	}

	@Test
	public void testCreateNewOntology_emptyIRI() {

		this.goh_raw.createNewOntology(""); //TODO: wie ist das erwartete Verhalten?
		assertNotNull(this.goh_raw.getOntology());		
	}
	
	@Test
	public void testCreateNewOntology_failDuplicate()  {
		this.goh_raw.createNewOntology(GenericOntologyHelperTest.TESTIRI);
		assertNotNull(this.goh_raw.getOntology());

		try
		{
			this.goh_raw.createNewOntology(GenericOntologyHelperTest.TESTIRI);
			fail("Eigentlich hätte eine Exception wegen doppelter IRI kommen müssen");
		}
		catch (Exception e){
			// alles OK		
			//System.out.println(e.getClass());
		}
		
	}

	@Test
	public void testSaveOntologyToFile() {
		String ontFileName = "tempfiles/testSaveOntologyToFileFile.owl";
		this.goh_raw.createNewOntology(GenericOntologyHelperTest.TESTIRI);
		this.goh_raw.saveOntologyToFile(new File(ontFileName));
		
		int fileLength;
		File f = new File(ontFileName);
		fileLength = (int) f.length();
		assertEquals(fileLength, 566); //evtl. zu eng -> anpassen, wenn Fehler auftreten
	}

	@Test
	public void testSaveOntologyToFile_failEmptyName() {
		String ontFileName = "";
		this.goh_raw.createNewOntology(GenericOntologyHelperTest.TESTIRI);
		
		try{
			this.goh_raw.saveOntologyToFile(new File(ontFileName));
			fail("Keine Exception trotz leerem Dateinamen");
		}
		catch(OntologyHelperException e){
			// alles OK
		}
		
	}


	@Test
	public void testSaveOntologyToFile_failNull() {		
		this.goh_raw.createNewOntology(GenericOntologyHelperTest.TESTIRI);
		
		try{
			this.goh_raw.saveOntologyToFile(null);
			fail("Keine Exception trotz null");
		}
		catch(NullPointerException e){
			// alles OK
		}		
	}

	@Test
	public void testLoadOWLOntologyFromFile() {
		String inFileName = "resources/models/Entire_Law_Ontology.owl";
		
		this.goh_raw.loadOWLOntologyFromFile(new File(inFileName));
		assertNotNull(this.goh_raw.getOntology());

		String outFileName = "tempfiles/testResave.owl";
		this.goh_raw.saveOntologyToFile(new File(outFileName));
		assertEquals(new File(inFileName).length(), new File(outFileName).length());
		// TODO: die Dateien sollten auch inhaltlich identisch sein
		
	}

	@Test
	public void testLoadOWLOntologyFromFile_failMissingFile() {
		String inFileName = "resources/models/bla.owl";
		
		try{
		this.goh_raw.loadOWLOntologyFromFile(new File(inFileName));
		fail("Die Datei sollte es nicht geben.");
		}
		catch (OWLOntologyInputSourceException e){
			// alles oK
		}

	}

	@Test
	public void testLoadOWLOntologyFromFile_failCorruptFile() {
		String inFileName = "resources/models/invalid.owl";
		
		try{
		this.goh_raw.loadOWLOntologyFromFile(new File(inFileName));
		
		fail("Die Datei Datei ist eigentlich nicht im OWL-Format.");
		}
		catch (OntologyHelperException e){
			// alles oK
		}

	}
	


	@Test
	public void testSetPrefix() {
		this.goh_raw.setPrefix("");
		this.goh_raw.setPrefix("http://carisma.umlsec.de/ontologies/regulatory/");
		this.goh_raw.setPrefix("\"\\\\\"");
		//Wenigstens keine Exception
		//TODO: erzeugte PrefixManager lassen sich nicht abfragen
	}


	@Test
	public void testGetOWLClass() {
		this.goh_raw.setPrefix(TESTIRI);
		OWLClass testClass1 = this.goh_raw.getOWLClass("testClass1");
		assertNotNull(testClass1);
	}

	@Test
	public void testCreateSubclassOfAxiom() {
		
		this.goh_init.createSubclassOfAxiom("subClass1", "superClass1");

		this.goh_init.saveOntologyToFile(new File("tempfiles/testCreateSubclassOfAxiom.owl"));
	}
	
	//TODO: fail-Test mit nicht initialisierter Ontologie


	@Test
	public void testCreateOWLIndividual() {
		this.goh_init.createOWLIndividual("individual1","class1");
		this.goh_init.saveOntologyToFile(new File("tempfiles/testCreateOWLIndividual.owl"));
	}
	
	@Test
	public void testGetOWLAnnotationProperty() {
		OWLAnnotationProperty prop1 = this.goh_init.getOWLAnnotationProperty("testAnnotationProperty");
		assertNotNull(prop1);
	}

	

	@Test
	public void testGetOWLObjectProperty() {
		OWLObjectProperty prop1 = this.goh_init.getOWLObjectProperty("testObjectProperty");
		assertNotNull(prop1);
	}


	
	@Test
	public void testGetIndividuals() {
		this.goh_init.createSubclassOfAxiom("class3", "class2");
		this.goh_init.createOWLIndividual("ind1", "class1");
		this.goh_init.createOWLIndividual("ind2", "class2");
		this.goh_init.createOWLIndividual("ind3", "class2");
		this.goh_init.createOWLIndividual("ind4", "class3");
		this.goh_init.createOWLIndividual("ind5", "class3");
		this.goh_init.createOWLIndividual("ind6", "class3");
		this.goh_init.createOWLIndividual("ind7", "class3");
		
		Set<OWLNamedIndividual> set1 = this.goh_init.getIndividuals("class1", true);
		assertEquals(set1.size(), 1);
		OWLNamedIndividual ind1 = set1.iterator().next();		
		assertEquals(ind1.getIRI().getFragment(), "ind1");

		// nur class2: 2 Individuen
		Set<OWLNamedIndividual> set2 = this.goh_init.getIndividuals("class2", true);
		assertEquals(set2.size(), 2);

		// class2 und class3: zusammen 6 Individuen
		Set<OWLNamedIndividual> set3 = this.goh_init.getIndividuals("class2", false);
		assertEquals(set3.size(), 6);
	}
	@Test
	public void testGetIndividualByName() {
		this.goh_init.createSubclassOfAxiom("class3", "class2");
		this.goh_init.createOWLIndividual("ind1", "class1");
		this.goh_init.createOWLIndividual("ind2", "class2");
		this.goh_init.createOWLIndividual("ind3", "class2");
		this.goh_init.createOWLIndividual("ind4", "class3");
		this.goh_init.createOWLIndividual("ind5", "class3");
		this.goh_init.createOWLIndividual("ind6", "class3");
		this.goh_init.createOWLIndividual("ind7", "class3");
		
		assertNotNull(this.goh_init.getIndividualById("ind1"));
		assertNotNull(this.goh_init.getIndividualById("ind3"));
		assertNotNull(this.goh_init.getIndividualById("ind6"));
		
		assertNull(this.goh_init.getIndividualById("bla"));		
	}


	@Test
	public void testCreateSimpleRelationship() {
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");
		OWLNamedIndividual ind2 = this.goh_init.createOWLIndividual("Ind2", "class2");
		
		this.goh_init.createSimpleRelationship(ind1, ind2, "rel1"); // TODO Ueberpruefung der Relation??
		this.goh_init.saveOntologyToFile(new File("tempfiles/testCreateSimpleRelationship.owl"));
	}


	@Test
	public void testCreateAnnotationOWLNamedIndividualStringString() {
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");
		
		this.goh_init.createAnnotation(ind1, "annotationname1", "annotationValue");
		this.goh_init.saveOntologyToFile(new File("tempfiles/testCreateAnnotationOWLNamedIndividualStringString.owl"));
	}
	

	@Test
	public void testGetStringAnnotation() {
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");		
		this.goh_init.createAnnotation(ind1, "annotationname1", "annotationValue");

		try {
			String result = this.goh_init.getStringAnnotation(ind1, "annotationname1");
			assertEquals(result, "annotationValue");
		} catch (NoSuchPropertyException e) {
			fail();			
		}
		
		try {
			String result = this.goh_init.getStringAnnotation(ind1, "gibtsnicht");
			fail("Wert gibt es eigentlich nicht");
		} catch (NoSuchPropertyException e) {
			// gut so
		}		
	}


	@Test
	public void testCreateAnnotationOWLNamedIndividualStringFloat() {
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");
		
		this.goh_init.createAnnotation(ind1, "annotationname1", 3.14159f);
		this.goh_init.saveOntologyToFile(new File("tempfiles/testCreateAnnotationOWLNamedIndividualStringFloat.owl"));

	}

	@Test
	public void testGetFloatAnnotation() {
		float testValue = 2.7182f;
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");		
		this.goh_init.createAnnotation(ind1, "annotationname1", testValue);

		try {
			float result = this.goh_init.getFloatAnnotation(ind1, "annotationname1");
			assertEquals(result, testValue, 0);
		} catch (NoSuchPropertyException e) {
			fail();			
		}
		
		try {
			String result = this.goh_init.getStringAnnotation(ind1, "gibtsnicht");
			fail("Wert gibt es eigentlich nicht");
		} catch (NoSuchPropertyException e) {
			// gut so
		}
	}

	@Test
	public void testCreateAnnotationOWLNamedIndividualStringBoolean() {
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");
		
		this.goh_init.createAnnotation(ind1, "annotationname1", true);
		this.goh_init.createAnnotation(ind1, "annotationname2", false);
		this.goh_init.saveOntologyToFile(new File("tempfiles/testCreateAnnotationOWLNamedIndividualStringBoolean.owl"));
	}


	@Test
	public void testGetBooleanAnnotation() {
		boolean testValue = true;
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");		
		this.goh_init.createAnnotation(ind1, "annotationname1", testValue);

		try {
			boolean result = this.goh_init.getBooleanAnnotation(ind1, "annotationname1");
			assertEquals(result, testValue);
		} catch (NoSuchPropertyException e) {
			fail();			
		}
		
		try {
			boolean result = this.goh_init.getBooleanAnnotation(ind1, "gibtsnicht");
			fail("Wert gibt es eigentlich nicht");
		} catch (NoSuchPropertyException e) {
			// gut so
		}

	}

	@Test
	public void testCreateAnnotationOWLNamedIndividualStringInt() {
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");
		
		this.goh_init.createAnnotation(ind1, "annotationname1", 42);		
		this.goh_init.saveOntologyToFile(new File("tempfiles/testCreateAnnotationOWLNamedIndividualStringInt.owl"));
	}

	@Test
	public void testGetIntegerAnnotation() {
		int testValue = 561;
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");		
		this.goh_init.createAnnotation(ind1, "annotationname1", testValue);

		try {
			int result = this.goh_init.getIntegerAnnotation(ind1, "annotationname1");
			assertEquals(result, testValue);
		} catch (NoSuchPropertyException e) {
			fail();			
		}
		
		try {
			int result = this.goh_init.getIntegerAnnotation(ind1, "gibtsnicht");
			fail("Wert gibt es eigentlich nicht");
		} catch (NoSuchPropertyException e) {
			// gut so
		}

	}
	
	@Test
	public void testRemoveAnnotation() {
		int testValue = 561;
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");		
		this.goh_init.createAnnotation(ind1, "annotationname1", testValue);

		try {
			int result = this.goh_init.getIntegerAnnotation(ind1, "annotationname1");
			assertEquals(result, testValue);
		} catch (NoSuchPropertyException e) {
			fail();			
		}
		
		this.goh_init.removeAnnotation(ind1, "annotationname1");
		
		try {
			int result = this.goh_init.getIntegerAnnotation(ind1, "annotationname1");
			fail("Wert gibt es eigentlich nicht");
		} catch (NoSuchPropertyException e) {
			// gut so
		}
		
	}

	@Test
	public void testGetRelatedIndividuals() {
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");
		OWLNamedIndividual ind2 = this.goh_init.createOWLIndividual("Ind2", "class2");		
		this.goh_init.createSimpleRelationship(ind1, ind2, "rel1");
		
		Set<OWLNamedIndividual> result = this.goh_init.getRelatedIndividuals(ind1, "rel1");
		assertEquals(result.size(), 1);
		result = this.goh_init.getRelatedIndividuals(ind2, "rel1");
		assertEquals(result.size(), 0);
		
		try{
		result = this.goh_init.getRelatedIndividuals(ind1, "bla");
		//fail("Exception fehlt"); // TODO es wird null zurückgegeben, daher
		assertNull(result);
		}
		catch(NullPointerException e)
		{}
	}


	@Test
	public void testGetInverseRelatedIndividuals() {
		OWLNamedIndividual ind1 = this.goh_init.createOWLIndividual("Ind1", "class1");
		OWLNamedIndividual ind2 = this.goh_init.createOWLIndividual("Ind2", "class2");		
		this.goh_init.createSimpleRelationship(ind1, ind2, "rel1");
		
		Set<OWLNamedIndividual> result = this.goh_init.getInverseRelatedIndividuals(ind2, "rel1");
		assertEquals(1, result.size());
		result = this.goh_init.getInverseRelatedIndividuals(ind1, "rel1");
		assertEquals(result.size(), 0);
		
		try{
		result = this.goh_init.getInverseRelatedIndividuals(ind1, "bla");
		fail("Exception fehlt");
		}
		catch(NullPointerException e)
		{}

	}
	
	public static void main(String[] args) throws Exception {
		IRI ontologyIRI = IRI.create("http://www.co-ode.org/test.owl");
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		OWLDataFactory dataFactory = manager.getOWLDataFactory();
		OWLOntology ontology = manager.createOntology(ontologyIRI);
		PrefixManager prefixManager = new DefaultPrefixManager("http://www.co-ode.org/");

		OWLClass person = dataFactory.getOWLClass(":Person", prefixManager);
		OWLNamedIndividual mary = dataFactory.getOWLNamedIndividual(":Mary", prefixManager);
		OWLClassAssertionAxiom classAssertion_mary = dataFactory.getOWLClassAssertionAxiom(person, mary);
		manager.addAxiom(ontology, classAssertion_mary);
		
		OWLNamedIndividual paul = dataFactory.getOWLNamedIndividual(":Paul", prefixManager);
		OWLClassAssertionAxiom classAssertion_paul = dataFactory.getOWLClassAssertionAxiom(person, paul);
		manager.addAxiom(ontology, classAssertion_paul);
		
		
		OWLObjectProperty property = dataFactory.getOWLObjectProperty(IRI.create("http://www.co-ode.org/prop_1"));
		OWLObjectPropertyAssertionAxiom opaa = dataFactory.getOWLObjectPropertyAssertionAxiom(property, mary, paul);
		
		

		// die Annotation, die an diie Property geschrieben wird:
		OWLAnnotationProperty annotationProperty = dataFactory.getOWLAnnotationProperty(IRI.create("http://www.co-ode.org/annotationProp1"));
		OWLAnnotationValue annotationValue = dataFactory.getOWLLiteral("lksadjfklasdjflk");
		OWLAnnotation annotation = dataFactory.getOWLAnnotation(annotationProperty, annotationValue);
		
		HashSet<OWLAnnotation> set = new HashSet<OWLAnnotation>();
		set.add(annotation);

		// wichtig:
		opaa = (OWLObjectPropertyAssertionAxiom) opaa.getAnnotatedAxiom(set);
		
		System.out.println(opaa.getAnnotations());//.add(annotation);
		
		manager.addAxiom(ontology, opaa);
		
//		OWLAnnotationProperty annotationProperty = dataFactory.getOWLAnnotationProperty(IRI.create("http://www.co-ode.org/annotationProp1"));
//		OWLAnnotationValue annotationValue = dataFactory.getOWLLiteral("asdf");
//		OWLAnnotation annotation = dataFactory.getOWLAnnotation(annotationProperty, annotationValue);
//		HashSet<OWLAnnotation> set = new HashSet<OWLAnnotation>();
//		set.add(annotation);
//		
//		OWLAnnotationAssertionAxiom aaai = new OWLAnnotationAssertionAxiomImpl(mary,annotationProperty,annotationValue,set);
//				
//				//(dataFactory,mary,annotationProperty,annotationValue,set);
//				
//
//		public OWLAnnotationAssertionAxiomImpl(OWLAnnotationSubject subject, OWLAnnotationProperty property, OWLAnnotationValue value, Collection<? extends OWLAnnotation> annotations) {

		
		
		
		
		
		manager.saveOntology(ontology, IRI.create(new File("tempfiles/testOntology.owl")));		
		System.out.println("fertig");
	}
	
	
	@Test
	public void testGetAnnotationsFromAxiom() {
		Set<Annotation> annotations = new HashSet<Annotation>();
	    annotations.add(new Annotation(RegulatoryOntologyHelper.PROP_REL_RULEELEMENTS_RULEELEMENTS_TYPE, "bla"));
	    annotations.add(new Annotation(RegulatoryOntologyHelper.PROP_REL_RULEELEMENTS_RULEELEMENTS_DESCRIPTION, "blupp"));
		OWLNamedIndividual source = goh_init.createOWLIndividual("12345", RegulatoryOntologyHelper.CLS_ACTIVITY);
		OWLNamedIndividual target = goh_init.createOWLIndividual("54321", RegulatoryOntologyHelper.CLS_ARTIFACT);
		OWLAxiom axiom = goh_init.createSimpleRelationship(source, target, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS, annotations);
		Set<OWLAnnotation> annotations1 = goh_init.getAnnotationsFromAxiom(axiom, RegulatoryOntologyHelper.PROP_REL_RULEELEMENTS_RULEELEMENTS_TYPE);
		assertEquals("Size of the returned set is not equal to 1. Size was " + annotations1.size(), 1, annotations1.size());
		for (OWLAnnotation annotation : annotations1) {
			assertEquals("bla", goh_init.owlAnnotationValueToString(annotation.getValue()));
		}
		
	}
	

}
