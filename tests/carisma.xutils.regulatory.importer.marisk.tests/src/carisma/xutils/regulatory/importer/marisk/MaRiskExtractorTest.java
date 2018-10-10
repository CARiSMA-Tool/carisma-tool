package carisma.xutils.regulatory.importer.marisk;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.Node;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import carisma.regulatory.ontology.utils.GenericOntologyHelper;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class MaRiskExtractorTest {

	/** first Test Method to get a better understanding of the importer.
	 * 
	 */
	@Test
	public void handleNodesTest() {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		roh.createNewRegulatoryOntology();

		Document document = DocumentHelper.createDocument();
        Element root = document.addElement( "root" );

        Element firstElement = root.addElement("h2");
        firstElement.addElement("strong")
				.addText("1. SomePlainText");
        
        firstElement.addElement("td");
        
        Element firstBinding = firstElement.addElement("td");
        firstBinding.setText("4.5 BINDING");
        Element firstComment = firstElement.addElement("td");
        firstComment.setText("2.3 superComment");
        
        Element firstElementFirstSub = root.addElement("h3");
        firstElementFirstSub.addElement("strong").addText("1.1 blub");
        Element firstElementSecondSub = root.addElement("h3");
        firstElementSecondSub.addElement("strong").addText("1.2 bla");
        
        
        Element secondRule = root.addElement("h2");
        secondRule.addElement("strong").addText("2. Crazy Stuff");

        Element secondBinding = secondRule.addElement("td");
        secondBinding.addText("Hier ist ein Test");
        Element secondComment = secondRule.addElement("td");
        secondComment.setText("1.3 SecondComment");
        secondRule.addElement("td");
        Element secondCommentB = secondRule.addElement("td");
        secondCommentB.addText("neuer Test");
        Element thirdBinding = secondRule.addElement("td");
        thirdBinding.addText("4. lskdjflskdjf");
        
        callMethod(document, roh, "Test_Onto");
	}
	
	/** Tests the handleNodes method.
	 * Expected output is a ontology with a single MARiskClause.  
	 * 
	 */
	@Test
	public void handleNodesTestOneClause() {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		roh.createNewRegulatoryOntology();

		Document document = DocumentHelper.createDocument();
        Element root = document.addElement( "root" );

        Element firstElement = root.addElement("h2");
        firstElement.addElement("strong").addText("1. SomePlainText");
        
        callMethod(document, roh, "OneClause");
		assertNotNull(getIndividualByNameAndClass("SomePlainText", RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME, RegulatoryOntologyHelper.CLS_MARISKCLAUSE, goh));
	}
	
	/** Tests the handleNodes method.
	 * Expected output is a ontology with a MARiskClause which has two Entries. One Binding and one Comment.
	 */
	@Test
	public void handleNodesOneClause_Binding_CommentTest() {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		roh.createNewRegulatoryOntology();

		Document document = DocumentHelper.createDocument();
        Element root = document.addElement( "root" );

        Element firstElement = root.addElement("h2");
        firstElement.addElement("strong")
				.addText("1. SomePlainText");
        
        firstElement.addElement("td");
        
        Element firstBinding = firstElement.addElement("td");
        firstBinding.setText("BINDING");
        Element firstComment = firstElement.addElement("td");
        firstComment.setText("2.3 superComment");
        
        callMethod(document, roh, "Test_Onto");
        
        OWLIndividual spt = getIndividualByNameAndClass("SomePlainText", RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME, RegulatoryOntologyHelper.CLS_MARISKCLAUSE, goh);
        Map<OWLObjectPropertyExpression, Set<OWLIndividual>> obProp = spt.getObjectPropertyValues(goh.getOntology());
        
        Set<OWLIndividual> entries = obProp.get(goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASENTRIES));
        
        assertTrue(entries.contains(getIndividualByNameAndClass("BINDING", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, RegulatoryOntologyHelper.CLS_MARISKBINDING, goh)));
        assertTrue(entries.contains(getIndividualByNameAndClass("2.3 superComment", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, RegulatoryOntologyHelper.CLS_MARISKCOMMENT, goh)));
	}
	
	/** Tests the handleNodes method.
	 * Expected output is a ontology with a MARiskClause which has three Entries. Two Binding,but only the second has a corresponding Comment.
	 */
	@Test
	public void handleNodesOneClause_TwoBindings_CommentTest() {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		roh.createNewRegulatoryOntology();

		Document document = DocumentHelper.createDocument();
        Element root = document.addElement( "root" );

        Element firstElement = root.addElement("h2");
        firstElement.addElement("strong")
				.addText("1. SomePlainText");
        
        firstElement.addElement("td");
        
        Element firstBinding = firstElement.addElement("td");
        firstBinding.setText("first binding");
        firstElement.addElement("td");  // Empty cell
        Element secondBinding = firstElement.addElement("td");
        secondBinding.setText("second binding");
        Element firstComment = firstElement.addElement("td");
        firstComment.setText("Comment");
        
        callMethod(document, roh, "Test_Onto");
        
        OWLIndividual spt = getIndividualByNameAndClass("SomePlainText", RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME, RegulatoryOntologyHelper.CLS_MARISKCLAUSE, goh);
        Map<OWLObjectPropertyExpression, Set<OWLIndividual>> obProp = spt.getObjectPropertyValues(goh.getOntology());
        
        Set<OWLIndividual> Clause_HasEntries_entries = obProp.get(goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASENTRIES));

        assertTrue(Clause_HasEntries_entries.contains(getIndividualByNameAndClass("first binding", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, RegulatoryOntologyHelper.CLS_MARISKBINDING, goh)));
        assertTrue(Clause_HasEntries_entries.contains(getIndividualByNameAndClass("second binding", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, RegulatoryOntologyHelper.CLS_MARISKBINDING, goh)));
        OWLIndividual comment = getIndividualByNameAndClass("Comment", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, RegulatoryOntologyHelper.CLS_MARISKCOMMENT, goh);
        assertTrue(Clause_HasEntries_entries.contains(comment));

        try {
			assertEquals("2", goh.getStringAnnotation(comment.asOWLNamedIndividual(), RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER));
		} catch (NoSuchPropertyException e) {
			fail("Couldn't find Annotation " + RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER + " at " + comment.toStringID());
		}
	}
	
	/** Tests the handleNodes method.
	 * Expected output is a ontology with a MARiskClause which has one SubClause with one Binding + Comment.
	 */
	@Test
	public void handleNodesOneClause_OneSubClauseTest() {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
		roh.createNewRegulatoryOntology();

		Document document = DocumentHelper.createDocument();
        Element root = document.addElement( "root" );

        Element firstElement = root.addElement("h2");
        firstElement.addElement("strong")
				.addText("1. SomePlainText");
        
        firstElement.addElement("td");
        
        Element subClause = root.addElement("h3");
        subClause.addElement("strong").addText("1.1 I am a subclause.");
        
        Element firstBinding = subClause.addElement("td");
        firstBinding.setText("1. first binding");
        Element firstComment = subClause.addElement("td");
        firstComment.setText("Comment");
        
        callMethod(document, roh, "SubClauseTest");
        
        OWLIndividual spt = getIndividualByNameAndClass("SomePlainText", RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME, RegulatoryOntologyHelper.CLS_MARISKCLAUSE, goh);
        Map<OWLObjectPropertyExpression, Set<OWLIndividual>> obProp = spt.getObjectPropertyValues(goh.getOntology());
        
        Set<OWLIndividual> sptSubClauses = obProp.get(goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASSUBCLAUSES));
        assertEquals(1,sptSubClauses.size());
        OWLIndividual sptSubClause = getIndividualByNameAndClass("I am a subclause.", RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME, RegulatoryOntologyHelper.CLS_MARISKCLAUSE, goh);
        assertTrue(sptSubClauses.contains(sptSubClause));
        
        Map<OWLObjectPropertyExpression, Set<OWLIndividual>> sptSubClausesObjecProperties = sptSubClause.getObjectPropertyValues(goh.getOntology());
        Set<OWLIndividual> sptSubClauseEntries = sptSubClausesObjecProperties.get(goh.getOWLObjectProperty(RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASENTRIES));
        assertEquals(2, sptSubClauseEntries.size());
        OWLIndividual sptSubClauseComment = getIndividualByNameAndClass("Comment", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, RegulatoryOntologyHelper.CLS_MARISKCOMMENT, goh);
        OWLIndividual sptSubClauseBinding = getIndividualByNameAndClass("first binding", RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT, RegulatoryOntologyHelper.CLS_MARISKBINDING, goh);
        assertTrue(sptSubClauseEntries.contains(sptSubClauseComment));
        assertTrue(sptSubClauseEntries.contains(sptSubClauseBinding));
	}
	
	
	
	
	// --------------  Helper methods ----------------
	
	/** returns an Individual by a given class and a Annotation Property.
	 * if no such Individual is found the test which called this method fails.
	 * @param propertyValue Value of the specified AnnotationProperty.
	 * @param propertyType Name of the property which is used for comparison.
	 * @param className Name of the Class.
	 * @param goh GenericOntologyHelper.
	 * @return returns an Individual iff it exists otherwise it returns nothing because the test fails.
	 */
	private OWLIndividual getIndividualByNameAndClass(String propertyValue, String propertyType, String className, GenericOntologyHelper goh) {

		OWLClass owlClass = goh.getOWLClass(className);
		assertNotNull(owlClass);
		
		for (OWLIndividual indi : owlClass.getIndividuals(goh.getOntology())) {
			try {
				if (goh.getStringAnnotation(indi.asOWLNamedIndividual(), propertyType).equals(propertyValue)) {
					return indi;
				}
			} catch (NoSuchPropertyException e) {
				e.printStackTrace();
				fail("Unexpected failure.");
			}
		}
		fail("Missing " + className + " with " + propertyType + ": " + propertyValue + ".");
		return null;
	}
	
	/** Helper to create everything necessary and to call the handelNodes({@link List}, {@link RegulatoryOntologyHelper}) method.
	 * 
	 * @param document Document from which the nodes shall be created
	 * @param roh {@link RegulatoryOntologyHelper}
	 * @param ontoName Name which will be used to save the ontology. WITHOUT the file extension.
	 */
	private void callMethod(Document document, RegulatoryOntologyHelper roh, String ontoName) {
		MaRiskExtractor extr = new MaRiskExtractor();
        @SuppressWarnings("unchecked")
		List<Node> nodes = document.selectNodes("//*"); 
        
		Class<? extends MaRiskExtractor> maRiskExtractorClass = extr.getClass();
		try {
			Method method = maRiskExtractorClass.getDeclaredMethod("handleNodes", List.class, RegulatoryOntologyHelper.class);
			method.setAccessible(true);
			method.invoke(extr, nodes, roh);
		} catch (Exception e) {
			e.printStackTrace();
			fail(e.getMessage());
		}
		

//		roh.saveOntologyToFile(new File("resources/" + ontoName + ".owl"));
	}
	
	/**
	 * Example which uses the whole .html file in resources.
	 */
//	@Test
//	public void extractTest() {
//		MaRiskExtractor extr = new MaRiskExtractor();
//		GenericOntologyHelper goh = new GenericOntologyHelper();
//		RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);
//		roh.createNewRegulatoryOntology();
//		extr.extract(new File("resources" + File.separator), roh);
//		roh.saveOntologyToFile(new File("resources/Test_OntoAll.owl"));
//	}
//	
}
