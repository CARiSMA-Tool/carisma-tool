package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Constraint;
import carisma.regulatory.ontology.Rule;
import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.Situation;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public class OWL2Factory {

	public static Collection<Rule> createRulesFromIndividuals(OWL2Ontology ontology, Set<OWLNamedIndividual> individuals) {
		ArrayList<Rule> rules = new ArrayList<Rule>();
		for (OWLNamedIndividual i : individuals) {
			rules.add(createRuleFromIndividual(ontology, i));
		}
		return rules;
	}
	
	public static Rule createRuleFromIndividual(OWL2Ontology ontology, OWLNamedIndividual individual) {
		for (OWLClassAssertionAxiom ocaa : ontology.getOntology().getClassAssertionAxioms(individual)) {
			if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_LAW)) {
				return new OWL2Law(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_PARAGRAPH)) {
				return new OWL2Paragraph(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_SECTION)) {
				return new OWL2Section(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_BSIELEMENT)) {
				return new OWL2BSIElement(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_BSIMEASURE)) {
				return new OWL2BSIMeasure(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_BSITHREAT)) {
				return new OWL2BSIThreat(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_MARISKBINDING)) {
                return new OWL2MariskBinding(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_MARISKCLAUSE)) {
                return new OWL2MariskClause(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_MARISKCOMMENT)) {
                return new OWL2MariskComment(ontology, individual);
			}
		}
		return new OWL2Rule(ontology, individual);
	}
	
	public static Collection<RuleElement> createRuleElementsFromIndividuals(OWL2Ontology ontology, Set<OWLNamedIndividual> individuals) {
        List<RuleElement> ruleElements = new ArrayList<RuleElement>();
        for (OWLNamedIndividual i : individuals) {
            ruleElements.add(createRuleElementFromIndividual(ontology, i));
        }
        return ruleElements;	    
	}
	
	public static RuleElement createRuleElementFromIndividual(OWL2Ontology ontology, OWLNamedIndividual individual) {
        for (OWLClassAssertionAxiom ocaa : ontology.getOntology().getClassAssertionAxioms(individual)) {
            if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().
                    equals(RegulatoryOntologyHelper.CLS_ACTIVITY)) {
                return new OWL2Activity(ontology, individual);
            } else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().
                    equals(RegulatoryOntologyHelper.CLS_ARTIFACT)) {
                return new OWL2Artifact(ontology, individual);
            } else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().
                    equals(RegulatoryOntologyHelper.CLS_ROLE)) {
                return new OWL2Role(ontology, individual);
            } else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().
                    equals(RegulatoryOntologyHelper.CLS_PROCESS)) {
                return new OWL2Process(ontology, individual);
            } else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().
                    equals(RegulatoryOntologyHelper.CLS_PROPERTY)) {
                return new OWL2Property(ontology, individual);
            } 
        }
        return new OWL2RuleElement(ontology, individual);	    
	}
	
	public static Rule createMariskRuleFromIndividual(OWL2Ontology ontology, OWLNamedIndividual individual) {
		for (OWLClassAssertionAxiom ocaa : ontology.getOntology().getClassAssertionAxioms(individual)) {
			if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_MARISKCLAUSE)) {
				return new OWL2MariskClause(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_MARISKBINDING)) {
				return new OWL2MariskBinding(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_MARISKCOMMENT)) {
				return new OWL2MariskComment(ontology, individual);
			} else if (ocaa.getClassExpression().asOWLClass().getIRI().getFragment().equals(RegulatoryOntologyHelper.CLS_MARISKRULE)) {
				return new OWL2MariskRule(ontology, individual);
			} 
		}
		return new OWL2Rule(ontology, individual);
	}

	public static Collection<Situation> createSituationsFromIndividuals(OWL2Ontology ontology, Set<OWLNamedIndividual> individuals) {
		ArrayList<Situation> rules = new ArrayList<Situation>();
		for (OWLNamedIndividual i : individuals) {
			rules.add(createSituationFromIndividual(ontology, i));
		}
		return rules;
	}
	
    public static Situation createSituationFromIndividual(OWL2Ontology ontology, OWLNamedIndividual individual) {
        return new OWL2Situation(ontology, individual);
    }
    
	public static Constraint createConstraintFromIndividual(OWL2Ontology ontology, OWLNamedIndividual individual) {
	    return new OWL2Constraint(ontology, individual);
	}
	
}
