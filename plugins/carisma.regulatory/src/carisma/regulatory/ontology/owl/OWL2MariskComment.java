package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.MariskComment;

public class OWL2MariskComment extends OWL2MariskEntry implements MariskComment{

	OWL2MariskComment(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

}
