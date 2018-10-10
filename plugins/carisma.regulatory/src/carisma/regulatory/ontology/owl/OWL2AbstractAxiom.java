package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;

public class OWL2AbstractAxiom {

    /**
     * The container OWL2Ontology element.
     */
    protected OWL2Ontology parent;
    
    /**
     * The individual in the OWL ontology that is represented by this element.
     */
    protected OWLObjectPropertyAssertionAxiom axiom;
    
    OWL2AbstractAxiom(OWL2Ontology onto, OWLObjectPropertyAssertionAxiom axiom) {
        this.parent = onto;
        this.axiom = axiom;
    }
    
    /**
     * returns the individual which is represented by this element.
     * @return
     */
    public final OWLObjectPropertyAssertionAxiom getOwlObjectPropertyAssertionAxiom() {
        return this.axiom;
    }

}
