package carisma.regulatory.ontology.owl;

import java.util.Set;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.Paragraph;
import carisma.regulatory.ontology.Section;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;


public class OWL2Section extends OWL2LawRule implements Section {

	OWL2Section(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public Paragraph getParagraph() {
		Set<OWLNamedIndividual> set = parent.getGenericOntologyHelper().
		        getInverseRelatedIndividuals(individual, RegulatoryOntologyHelper.REL_PARAGRAPH_SECTIONS);
		if (set.size()!=1) {
			throw new InvalidOWLOntologyException("Section has no paragraph: "+individual);
		}
		return (Paragraph)OWL2Factory.createRuleFromIndividual(parent, set.iterator().next());
	}

	@Override
	public String getContent() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_SECTION_CONTENT);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("Section without content: "+individual.toString());
		}
	}

	@Override
	public String getNumber() {
		return getTitle();
	}


}
