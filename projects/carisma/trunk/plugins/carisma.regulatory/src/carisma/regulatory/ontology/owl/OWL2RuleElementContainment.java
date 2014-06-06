package carisma.regulatory.ontology.owl;


import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;

import carisma.regulatory.ontology.Rule;
import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.RuleElementContainment;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2RuleElementContainment extends OWL2AbstractAxiom implements RuleElementContainment{

    OWL2RuleElementContainment(final OWL2Ontology parent, final OWLObjectPropertyAssertionAxiom axiom) {
        super(parent, axiom);
    }

    @Override
    public RuleElement getRuleElement() {
        Set<OWLNamedIndividual> indivduals = axiom.getAxiomWithoutAnnotations().getIndividualsInSignature();
        RuleElement ruleElement = null;
        for (OWLNamedIndividual individual : indivduals) {
            if (parent.getGenericOntologyHelper()
                    .getTypeOfIndividual(individual)
                    .equals(RegulatoryOntologyHelper.CLS_RULEELEMENT)) {
               ruleElement = OWL2Factory.createRuleElementFromIndividual(parent, individual);
            }
        }
        return ruleElement;
    }

    @Override
    public Rule getRule() {
        Set<OWLNamedIndividual> indivduals = axiom.getAxiomWithoutAnnotations().getIndividualsInSignature();
        Rule rule = null;
        for (OWLNamedIndividual individual : indivduals) {
            if (parent.getGenericOntologyHelper()
                    .getTypeOfIndividual(individual)
                    .equals(RegulatoryOntologyHelper.CLS_RULE)) {
               rule = OWL2Factory.createRuleFromIndividual(parent, individual);
            }
        }
        return rule;
    }
    
    /**
     * returns a map of the referenced elements by this ruleElementContainment. Ruleelement is the ruleelement to 
     * which the reference points. Rule is the rule which the ruleelement containes.
     * If there occurs an exception, eg. the ruleelement is null, this method returns null.
     * @return
     */
    public Map<RuleElement, Rule> getReferencedElements() {
        Map<RuleElement, Rule> referencedElements = null;
        try {
            referencedElements = new HashMap<RuleElement, Rule>();
            referencedElements.put(getRuleElement(), getRule());
        } catch (Exception e) {
            System.err.println("Failed to put the elements in the map. " + e.getLocalizedMessage());
            referencedElements = null;
        }
        return referencedElements;
    }

    @Override
    public int getStart() {
        Set<OWLAnnotation> annotations = parent.getGenericOntologyHelper()
                .getAnnotationsFromAxiom(axiom, 
                        RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_STARTINDEX);
        if (annotations.size() == 1) {
            OWLAnnotation annotation = annotations.iterator().next();
            try {
                return parent.getGenericOntologyHelper().owlAnnotationValueToInteger(annotation.getValue());
            } catch (Exception e) {
                System.err.println("Failed to parse the annotation value. " + e.getLocalizedMessage());
                return 0;
            }
        } else {
            return 0;
        }
    }

    @Override
    public int getEnd() {
        Set<OWLAnnotation> annotations = parent.getGenericOntologyHelper()
                .getAnnotationsFromAxiom(axiom, 
                        RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_ENDINDEX);
        if (annotations.size() == 1) {
            OWLAnnotation annotation = annotations.iterator().next();
            try {
                return parent.getGenericOntologyHelper().owlAnnotationValueToInteger(annotation.getValue());
            } catch (Exception e) {
                System.err.println("Failed to parse the annotation value. " + e.getLocalizedMessage());
                return 0;
            }
        } else {
            return 0;
        }
    }

    @Override
    public String getRepresentation() {
        Set<OWLAnnotation> annotations = parent.getGenericOntologyHelper()
                .getAnnotationsFromAxiom(axiom, 
                        RegulatoryOntologyHelper.PROP_REL_RULE_RULEELEMENTS_REPRESENTATION);
        if (annotations.size() == 1) {
            OWLAnnotation annotation = annotations.iterator().next();
            try {
                return parent.getGenericOntologyHelper().owlAnnotationValueToString(annotation.getValue());
            } catch (Exception e) {
                System.err.println("Failed to parse the annotation value. " + e.getLocalizedMessage());
                return null;
            }            
        } else {
            return null;
        }
    }

}
