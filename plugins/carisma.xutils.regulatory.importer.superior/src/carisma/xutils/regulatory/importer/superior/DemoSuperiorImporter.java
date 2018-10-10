package carisma.xutils.regulatory.importer.superior;

import java.io.File;

import carisma.regulatory.ontology.Ontology;
import carisma.regulatory.ontology.owl.OWL2Ontology;
import carisma.regulatory.ontology.utils.GenericOntologyHelper;

// Test/Demo für die Verwendung der Klassen in carisma.regulatory.ontology.owl

public class DemoSuperiorImporter {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		GenericOntologyHelper goh = new GenericOntologyHelper();
		
		Ontology ontology = OWL2Ontology.loadFromFile(new File("resources/Empty_Ontology.owl"));
				
		((OWL2Ontology) ontology).getGenericOntologyHelper().saveOntologyToFile(new File("resources/New_Ontology.owl"));
		
		

	}

}
