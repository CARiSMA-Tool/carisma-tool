package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Constraint;
import carisma.regulatory.ontology.Rule;
import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.Situation;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public class OWL2Situation extends OWL2AbstractElement implements Situation {

	OWL2Situation(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

    @Override
    public String getName() {
        try {
            return parent.getGenericOntologyHelper().
                    getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_SITUATION_NAME);
        } catch (NoSuchPropertyException e) {
            throw new InvalidOWLOntologyException("Situation without a name: "+individual.toString());
        }
    }
    
	@Override
	public Collection<Rule> getInvolvedRules() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_SITUATION_INVOLVEDRULES);
		ArrayList<Rule> list = new ArrayList<Rule>();
		for (OWLNamedIndividual i : set) {
			Rule s = OWL2Factory.createRuleFromIndividual(parent, i);
			list.add(s);
		}
		return list;
	}

    @Override
    public Collection<RuleElement> getInvolvedRuleElements() {
        Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
                getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_SITUATION_INVOLVEDRULEELEMENTS);
        ArrayList<RuleElement> list = new ArrayList<RuleElement>();
        for (OWLNamedIndividual i : set) {
            RuleElement s = OWL2Factory.createRuleElementFromIndividual(parent, i);
            list.add(s);
        }
        return list;
    }

    @Override
    public Collection<Constraint> getConstraintsToCheck() {
        Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
                getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_SITUATION_CHECKED_BY);
        ArrayList<Constraint> list = new ArrayList<Constraint>();
        for (OWLNamedIndividual i : set) {
            Constraint s = OWL2Factory.createConstraintFromIndividual(parent, i);
            list.add(s);
        }
        return list;
    }

}
