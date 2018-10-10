package carisma.regulatory.ontology.owl;

import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.MariskBinding;
import carisma.regulatory.ontology.MariskComment;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2MariskBinding extends OWL2MariskEntry implements MariskBinding{

	public OWL2MariskBinding(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public MariskComment getComment() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_MARISKBINDING_HASCOMMENTS);
		if (set.size()!=1) {
			throw new InvalidOWLOntologyException("Binding has no comment: "+individual);
		}
		return (MariskComment)OWL2Factory.createMariskRuleFromIndividual(parent, set.iterator().next());
	}

}
