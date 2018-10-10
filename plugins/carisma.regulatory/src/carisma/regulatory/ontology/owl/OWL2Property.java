package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Property;

public class OWL2Property extends OWL2RuleElement implements Property {

    OWL2Property(OWL2Ontology onto, OWLNamedIndividual individual) {
        super(onto, individual);
    }

}
