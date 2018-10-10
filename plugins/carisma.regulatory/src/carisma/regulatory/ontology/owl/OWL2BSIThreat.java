package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.BSIThreat;


public class OWL2BSIThreat extends OWL2BSIRule implements BSIThreat {

	public OWL2BSIThreat(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

}
