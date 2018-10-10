package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

/**
 * Abstract superclass for all OWL2 implementations of the ontology elements. It stores a reference to the individual of the OWL ontology. 
 * @author wenzel
 *
 */
public class OWL2AbstractElement {

	/**
	 * The container OWL2Ontology element.
	 */
	protected OWL2Ontology parent;
	
	/**
	 * The individual in the OWL ontology that is represented by this element.
	 */
	protected OWLNamedIndividual individual;
	
	OWL2AbstractElement(OWL2Ontology onto, OWLNamedIndividual individual) {
		this.parent = onto;
		this.individual = individual;
	}
	
	/**
	 * returns the individual which is represented by this element.
	 * @return
	 */
	public final OWLNamedIndividual getOWLNamedIndividual() {
	    return this.individual;
	}

}
