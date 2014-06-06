package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.BSIThreat;


public class OWL2BSIMeasure extends OWL2BSIRule implements BSIThreat {

	public OWL2BSIMeasure(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

}
