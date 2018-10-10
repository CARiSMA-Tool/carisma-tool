package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.GuidelinesEntry;

public class OWL2GuidelinesEntry extends OWL2Guidelines implements GuidelinesEntry{

    OWL2GuidelinesEntry(OWL2Ontology onto, OWLNamedIndividual individual) {
        super(onto, individual);
    }

}
