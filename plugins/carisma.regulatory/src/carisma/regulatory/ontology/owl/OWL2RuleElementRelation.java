package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;

import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.RuleElementRelation;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2RuleElementRelation extends OWL2AbstractAxiom implements RuleElementRelation{

    OWL2RuleElementRelation(final OWL2Ontology onto, final OWLObjectPropertyAssertionAxiom axiom) {
        super(onto, axiom);
    }

    @Override
    public RuleElement getSource() {
        Set<OWLNamedIndividual> relatedIndividuals = axiom.getIndividualsInSignature();
        OWLNamedIndividual source = null; OWLNamedIndividual target = null;
        List<OWLNamedIndividual> list = new ArrayList<OWLNamedIndividual>(relatedIndividuals);
        if (list.size() == 2) {
            source = list.get(0);   // here should be retrieved the source individual
            target = list.get(1);
            Set<OWLNamedIndividual> relatedFromSource = parent.getGenericOntologyHelper()
                    .getRelatedIndividuals(source, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
            if (relatedFromSource.contains(target)) {
                return OWL2Factory.createRuleElementFromIndividual(parent, source);
            } else {
                return OWL2Factory.createRuleElementFromIndividual(parent, target);
            }
        } else {
            throw new InternalError("The list was corrupt. List size is " + list.size() + ", but should be 2!");
        }
    }

    @Override
    
    public RuleElement getTarget() {
        Set<OWLNamedIndividual> relatedIndividuals = axiom.getIndividualsInSignature();
        OWLNamedIndividual source = null; OWLNamedIndividual target = null;
        List<OWLNamedIndividual> list = new ArrayList<OWLNamedIndividual>(relatedIndividuals);
        if (list.size() == 2) {
            source = list.get(0);   
            target = list.get(1);   // here should be retrieved the target individual
            Set<OWLNamedIndividual> relatedFromSource = parent.getGenericOntologyHelper()
                    .getInverseRelatedIndividuals(source, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
            if (relatedFromSource.contains(source)) {
                return OWL2Factory.createRuleElementFromIndividual(parent, target);
            } else {
                return OWL2Factory.createRuleElementFromIndividual(parent, source);
            }
        } else {
            throw new InternalError("The list was corrupt. List size is " + list.size() + ", but should be 2!");
        }
    }

    @Override
    public String getType() {
        Set<OWLAnnotation> annotations = parent.getGenericOntologyHelper()
                .getAnnotationsFromAxiom(axiom, 
                        RegulatoryOntologyHelper.PROP_REL_RULEELEMENTS_RULEELEMENTS_TYPE);
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

    @Override
    public String getDescription() {
        Set<OWLAnnotation> annotations = parent.getGenericOntologyHelper()
                .getAnnotationsFromAxiom(axiom, 
                        RegulatoryOntologyHelper.PROP_REL_RULEELEMENTS_RULEELEMENTS_DESCRIPTION);
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
