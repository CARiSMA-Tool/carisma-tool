package carisma.regulatory.ontology.owl;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Law;
import carisma.regulatory.ontology.Paragraph;
import carisma.regulatory.ontology.Section;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public class OWL2Paragraph extends OWL2LawRule implements Paragraph {

	OWL2Paragraph(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public Law getLaw() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getInverseRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_LAW_PARAGRAPHS);
		if (set.size()!=1) {
			throw new InvalidOWLOntologyException("Section has no paragraph: "+individual);
		}
		return (Law)OWL2Factory.createRuleFromIndividual(parent, set.iterator().next());
	}

	@Override
	public List<Section> getSections() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_PARAGRAPH_SECTIONS);
		ArrayList<Section> list = new ArrayList<Section>();
		for (OWLNamedIndividual i : set) {
			Section s = (Section)OWL2Factory.createRuleFromIndividual(parent, i);
			list.add(s);
		}
		return list;
	}

	@Override
	public String getNumber() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_PARAGRAPH_NUMBER);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("Paragraph without number: "+individual.toString());
		}
	}

}
