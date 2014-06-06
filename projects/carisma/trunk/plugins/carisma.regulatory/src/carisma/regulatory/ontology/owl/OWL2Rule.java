package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;

import carisma.regulatory.ontology.Rule;
import carisma.regulatory.ontology.RuleElementContainment;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public class OWL2Rule extends OWL2AbstractElement implements Rule {

	OWL2Rule(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public String getTitle() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_RULE_TITLE);
		} catch (NoSuchPropertyException e) {
		    if (individual.getIRI().getFragment().contains("MARiskVA")) {
		        return "";        // Marisk individuals don't have a title
		    } else {
	            throw new InvalidOWLOntologyException("Rule without title: "+individual.toString());		        
		    }
		}
	}

	@Override
	public Collection<Rule> getReferredRules() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_RULE_REFFEREDRULES);
		ArrayList<Rule> list = new ArrayList<Rule>();
		for (OWLNamedIndividual i : set) {
			Rule s = OWL2Factory.createRuleFromIndividual(parent, i);
			list.add(s);
		}
		return list;
	}

	@Override
	public boolean isUnprovable() {
		try {
			return parent.getGenericOntologyHelper().
			        getBooleanAnnotation(individual, RegulatoryOntologyHelper.PROP_RULE_UNPROVABLE);
		} catch (NoSuchPropertyException e) {
			return false; // no property set means false
		}
	}

    @Override
    public Collection<RuleElementContainment> getContainedRuleElements() {
        Set<OWLAxiom> referecedAxioms = individual.getReferencingAxioms(parent.getOntology());
        Collection<RuleElementContainment> ruleElementContainments = new HashSet<RuleElementContainment>();
        for (OWLAxiom axiom : referecedAxioms) {
            if (axiom.toString().contains(RegulatoryOntologyHelper.REL_RULE_RULEELEMENTS)) {    // TODO check if cast works fine
                ruleElementContainments.add(new OWL2RuleElementContainment(parent, (OWLObjectPropertyAssertionAxiom) axiom));
            }
        }
        return ruleElementContainments;
    }

	
}
