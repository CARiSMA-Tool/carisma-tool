package carisma.regulatory.ontology.utils;

import java.io.File;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.profiles.OWL2DLProfile;
import org.semanticweb.owlapi.profiles.OWLProfileReport;
import org.semanticweb.owlapi.profiles.OWLProfileViolation;

public class TestOntologyWithOWL2Profile {

    /**
     * tests the Ontology with the help of OWL2Profile. Used Ontologies will not be saved.
     * @param args
     */
    public static void main(String[] args) {
        GenericOntologyHelper goh = new GenericOntologyHelper();
        goh.createNewOntology("resources" + File.separator + "TestOwlApi_Ontology.owl");
        RegulatoryOntologyHelper roh = new RegulatoryOntologyHelper(goh);    
        System.out.println("List the results:");
            // use of undeclared elements in the ontology
        System.out.println("---the Ontology where no class has been declared:");
        OWLNamedIndividual bsi1 = roh.createBSIElement("BSI_1", "BSI_1_Name", "Diese Text ist ein Test");
        roh.createSection(bsi1, "1", "content1");
        roh.assignConstraintToSituation(bsi1, bsi1);
        goh.createAnnotation(bsi1, "TestAnnotation", "test");
        goh.createSimpleRelationship(bsi1, bsi1, "self");
        roh.createContainedRuleElementsRelation(bsi1, bsi1, "textualRepresentation", 0, 0);
        checkOntology(roh.getOntology());
        
            // use of the generated Ontology in superior importer
        System.out.println("---the Ontology which has been generated from the importers:");
        goh.loadOWLOntologyFromFile(new File("../carisma.xutils.regulatory.importer.superior/resources/Entire_Law_Ontology.owl"));
        checkOntology(goh.getOntology());
        
    }
    
    
    
    
    private static void checkOntology(final OWLOntology ontology) {
        OWL2DLProfile profile = new OWL2DLProfile();
        OWLProfileReport report = profile.checkOntology(ontology);
        for(OWLProfileViolation v:report.getViolations()) {
        System.out.println(v);
        }
    }

}
