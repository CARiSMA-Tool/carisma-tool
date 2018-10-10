package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Guidelines;

public class OWL2Guidelines extends OWL2Rule implements Guidelines{

    OWL2Guidelines(OWL2Ontology onto, OWLNamedIndividual individual) {
        super(onto, individual);
    }
}
