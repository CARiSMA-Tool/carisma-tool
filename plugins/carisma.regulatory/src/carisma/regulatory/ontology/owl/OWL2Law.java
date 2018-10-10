package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Law;
import carisma.regulatory.ontology.Paragraph;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public class OWL2Law extends OWL2LawRule implements Law {

	OWL2Law(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public List<Paragraph> getParagraphs() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_LAW_PARAGRAPHS);
		ArrayList<Paragraph> list = new ArrayList<Paragraph>();
		for (OWLNamedIndividual i : set) {
			Paragraph s = (Paragraph)OWL2Factory.createRuleFromIndividual(parent, i);
			list.add(s);
		}
		return list;
	}

}
