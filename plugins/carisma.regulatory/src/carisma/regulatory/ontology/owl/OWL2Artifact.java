package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Property;
import carisma.regulatory.ontology.Artifact;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2Artifact extends OWL2RuleElement implements Artifact {

    public OWL2Artifact(OWL2Ontology onto, OWLNamedIndividual individual) {
        super(onto, individual);
    }

    @Override
    public List<Property> getProperties() {
        Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
                getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
        List<Property> list = new ArrayList<Property>();
        for (OWLNamedIndividual individual : set) {
            if (parent.getGenericOntologyHelper().getTypeOfIndividual(individual).equals("Property")) {
                Property s = (Property) OWL2Factory.createRuleElementFromIndividual(parent, individual);
                list.add(s);
            }
        }
        return list;
    }

}
