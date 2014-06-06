package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.MariskBinding;
import carisma.regulatory.ontology.MariskClause;
import carisma.regulatory.ontology.MariskEntry;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2MariskClause extends OWL2MariskRule implements MariskClause{

	public OWL2MariskClause(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public String getNumber() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NUMBER);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("Clause without number: "+individual.toString());
		}
	}

	@Override
	public String getTitle() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_MARISKCLAUSE_NAME);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("Clause without name: "+individual.toString());
		}
	}

	@Override
	public List<MariskEntry> getEntries() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASENTRIES);
		System.out.println("is the set empty? " + set.isEmpty());
		ArrayList<MariskEntry> list = new ArrayList<MariskEntry>();
		for (OWLNamedIndividual i : set) {
			MariskEntry s = (MariskEntry)OWL2Factory.createMariskRuleFromIndividual(parent, i);
			list.add(s);
		}
		return list;
	}

	@Override
	public List<MariskClause> getSubclauses() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_MARISKCLAUSE_HASSUBCLAUSES);
		ArrayList<MariskClause> list = new ArrayList<MariskClause>();
		for (OWLNamedIndividual i : set) {
			MariskClause s = (MariskClause)OWL2Factory.createMariskRuleFromIndividual(parent, i);
			list.add(s);
		}
		return list;
	}

}
