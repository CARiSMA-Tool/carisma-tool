package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.BSIElement;
import carisma.regulatory.ontology.BSIMeasure;
import carisma.regulatory.ontology.BSIThreat;
import carisma.regulatory.ontology.Paragraph;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public class OWL2BSIElement extends OWL2BSIRule implements BSIElement {

	public OWL2BSIElement(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public Collection<BSIThreat> getThreats() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_BSIELEMENT_THREATS);
		ArrayList<BSIThreat> list = new ArrayList<BSIThreat>();
		for (OWLNamedIndividual i : set) {
			BSIThreat s = (BSIThreat)OWL2Factory.createRuleFromIndividual(parent, i);
			list.add(s);
		}
		return list;
	}

	@Override
	public Collection<BSIMeasure> getMeasures() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_BSIELEMENT_MEASURES);
		ArrayList<BSIMeasure> list = new ArrayList<BSIMeasure>();
		for (OWLNamedIndividual i : set) {
			BSIMeasure s = (BSIMeasure)OWL2Factory.createRuleFromIndividual(parent, i);
			list.add(s);
		}
		return list;
	}

}
