package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Activity;
import carisma.regulatory.ontology.Artifact;
import carisma.regulatory.ontology.Property;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2Activity extends OWL2RuleElement implements Activity {
    
    public OWL2Activity(OWL2Ontology onto, OWLNamedIndividual individual) {
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

    @Override
    public List<Artifact> getUsedArtifacts() {
        Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
                getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
        List<Artifact> list = new ArrayList<Artifact>();
        for (OWLNamedIndividual individual : set) {
            if (parent.getGenericOntologyHelper().getTypeOfIndividual(individual).equals("Artifact")) {
                Artifact s = (Artifact) OWL2Factory.createRuleElementFromIndividual(parent, individual);
                list.add(s);
            }
        }
        return list;
    }
}
