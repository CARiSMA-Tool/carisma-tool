package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.LawRule;


public abstract class OWL2LawRule extends OWL2Rule implements LawRule {

	OWL2LawRule(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

}
