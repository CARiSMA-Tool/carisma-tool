package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Constraint;
import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2Constraint extends OWL2AbstractElement implements Constraint {

    OWL2Constraint(OWL2Ontology onto, OWLNamedIndividual individual) {
        super(onto, individual);
    }
    
 // TODO: remove when the file containing the mapping of constraints to carisma checks exist
    /**
     * Temporary method to return the type of the constraint so
     * that recommendations can be given.
     * @return
     */
// FIXME: only works if a constraint is only of one type
    @Override
    public String getType() {
        StringBuffer constraintTypes = new StringBuffer();
        for (OWLClassExpression typeExpression : individual.getTypes(parent.getOntology())) {
            constraintTypes.append(typeExpression.asOWLClass().getIRI().getFragment());
        }
        return constraintTypes.toString();
    }
    
   
    @Override
    public Collection<RuleElement> getParameters() {
        Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
                getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_CONSTRAINT_HASPARAMETERS);
        ArrayList<RuleElement> list = new ArrayList<RuleElement>();
        for (OWLNamedIndividual i : set) {
            RuleElement s = OWL2Factory.createRuleElementFromIndividual(parent, i);
            list.add(s);
        }
        return list;
    }

}
