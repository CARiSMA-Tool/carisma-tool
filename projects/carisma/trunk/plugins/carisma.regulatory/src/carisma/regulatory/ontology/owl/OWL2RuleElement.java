package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLOntology;


import carisma.regulatory.ontology.Rule;
import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.RuleElementContainment;
import carisma.regulatory.ontology.RuleElementRelation;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2RuleElement extends OWL2AbstractElement implements RuleElement{

	
	public OWL2RuleElement(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public String getElementType() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_RULEELEMENT_TYPE);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("Rule element without type: "+individual.toString());
		}
	}

	@Override
	public String getName(){
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_RULEELEMENT_NAME);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("Rule element without name: "+individual.toString());
		}
	}

    @Override
    public Collection<RuleElementRelation> getRelatedRuleElements() {
        Set<OWLAxiom> axioms = individual.getReferencingAxioms(parent.getOntology());
        Collection<RuleElementRelation> ruleElementRelations = new HashSet<RuleElementRelation>();
        for (OWLAxiom axiom : axioms) {         // TODO check if the cast works fine
            ruleElementRelations.add(new OWL2RuleElementRelation(parent, (OWLObjectPropertyAssertionAxiom) axiom));
        }
        return null;
    }

    @Override
    public Collection<RuleElementContainment> getContainingRules() {
        Collection<RuleElementContainment> ruleElementContainments = new HashSet<RuleElementContainment>();
        Set<OWLNamedIndividual> inverseRelatedRules = parent.getGenericOntologyHelper()
                                                        .getInverseRelatedIndividuals(individual, 
                                                                RegulatoryOntologyHelper.REL_RULE_RULEELEMENTS);
        for (OWLNamedIndividual individual : inverseRelatedRules) {
            Set<OWLAxiom> referecedAxioms = individual.getReferencingAxioms(parent.getOntology());
            for (OWLAxiom axiom : referecedAxioms) {
                if (axiom.toString().contains(RegulatoryOntologyHelper.REL_RULE_RULEELEMENTS)) {
                    ruleElementContainments.add(new OWL2RuleElementContainment(parent, (OWLObjectPropertyAssertionAxiom) axiom));
                }
            }
        }
        
        return ruleElementContainments;
    }

}
