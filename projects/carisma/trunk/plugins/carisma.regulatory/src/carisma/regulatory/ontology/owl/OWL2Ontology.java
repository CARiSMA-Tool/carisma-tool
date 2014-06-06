package carisma.regulatory.ontology.owl;

import java.io.File;
import java.util.Collection;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;

import carisma.regulatory.ontology.Ontology;
import carisma.regulatory.ontology.Rule;
import carisma.regulatory.ontology.RuleElement;
import carisma.regulatory.ontology.Situation;
import carisma.regulatory.ontology.utils.GenericOntologyHelper;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public class OWL2Ontology implements Ontology {

	private GenericOntologyHelper goh;
	public static Ontology loadFromFile(File file) {
		GenericOntologyHelper helper = new GenericOntologyHelper();
		helper.setPrefix(RegulatoryOntologyHelper.ONTOLOGY_IRI_BASE);
		helper.loadOWLOntologyFromFile(file);
		return new OWL2Ontology(helper);
	}
	
	protected OWL2Ontology(GenericOntologyHelper goh) {
		this.goh = goh;
	}
	
	OWLOntology getOntology() {
		return goh.getOntology();
	}
	
	public GenericOntologyHelper getGenericOntologyHelper() {
		return goh;
	}
	
	@Override
	public Collection<Rule> getRules() {
		Set<OWLNamedIndividual> inds = goh.getIndividuals(RegulatoryOntologyHelper.CLS_LAW, true);
		inds.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_MARISKBINDING, true));
		inds.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_MARISKCOMMENT, true));
		inds.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIELEMENT, true));
		inds.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSIMEASURE, true));
		inds.addAll(goh.getIndividuals(RegulatoryOntologyHelper.CLS_BSITHREAT, true));
		return (Collection<Rule>) OWL2Factory.createRulesFromIndividuals(this, inds);
	}

	@Override
	public Collection<Situation> getSituations() {
		Set<OWLNamedIndividual> inds = goh.getIndividuals(RegulatoryOntologyHelper.CLS_SITUATION, false);
		return (Collection<Situation>) OWL2Factory.createSituationsFromIndividuals(this, inds);
	}

    @Override
    public Collection<RuleElement> getRuleElements() {
        Set<OWLNamedIndividual> inds = goh.getIndividuals(RegulatoryOntologyHelper.CLS_RULEELEMENT, false);
        return (Collection<RuleElement>) OWL2Factory.createRuleElementsFromIndividuals(this, inds);
    }

}
