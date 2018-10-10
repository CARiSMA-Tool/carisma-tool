package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.MariskRule;


public class OWL2MariskRule extends OWL2Rule implements MariskRule{

	OWL2MariskRule(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}
}
