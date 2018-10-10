package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Activity;
import carisma.regulatory.ontology.Artifact;
import carisma.regulatory.ontology.Property;
import carisma.regulatory.ontology.Role;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2Role extends OWL2RuleElement implements Role {

    public OWL2Role(OWL2Ontology onto, OWLNamedIndividual individual) {
        super(onto, individual);
    }

    @Override
    public List<Activity> getPerformableActivities() {
        Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
                getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
        List<Activity> list = new ArrayList<Activity>();
        for (OWLNamedIndividual i : set) {
            if (parent.getGenericOntologyHelper().getTypeOfIndividual(i).equals("Activity")) {
                Activity s = (Activity) OWL2Factory.createRuleElementFromIndividual(parent, i);
                list.add(s);
            }
        }
        return list;
    }

    @Override
    public List<Artifact> getManagedArtifacts() {
        Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
                getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
        List<Artifact> list = new ArrayList<Artifact>();
        for (OWLNamedIndividual i : set) {
            if (parent.getGenericOntologyHelper().getTypeOfIndividual(i).equals("Artifact")) {
                Artifact s = (Artifact) OWL2Factory.createRuleElementFromIndividual(parent, i);
                list.add(s);
            }
        }
        return list;
    }

    @Override
    public List<Property> getProperties() {
        Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
                getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_RULEELEMENTS_RULEELEMENTS);
        List<Property> list = new ArrayList<Property>();
        for (OWLNamedIndividual i : set) {
            if (parent.getGenericOntologyHelper().getTypeOfIndividual(i).equals("Property")) {
                Property s = (Property) OWL2Factory.createRuleElementFromIndividual(parent, i);
                list.add(s);
            }
        }
        return list;
    }

}
