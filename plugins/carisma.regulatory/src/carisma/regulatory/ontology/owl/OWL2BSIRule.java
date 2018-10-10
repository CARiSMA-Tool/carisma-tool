package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.BSIRule;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public abstract class OWL2BSIRule extends OWL2Rule implements BSIRule {

	public OWL2BSIRule(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public String getContent() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_BSIRULE_CONTENT);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("BSIRule without content: "+individual.toString());
		}
	}

}
