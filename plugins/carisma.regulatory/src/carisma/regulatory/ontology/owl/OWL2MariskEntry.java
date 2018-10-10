package carisma.regulatory.ontology.owl;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import carisma.regulatory.ontology.MariskEntry;
import carisma.regulatory.ontology.utils.NoSuchPropertyException;
import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;

public class OWL2MariskEntry extends OWL2MariskRule implements MariskEntry{

	OWL2MariskEntry(OWL2Ontology onto, OWLNamedIndividual individual) {
		super(onto, individual);
	}

	@Override
	public String getNumber() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_MARISKENTRY_NUMBER);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("Entry without number: "+individual.toString());
		}
	}

	@Override
	public String getText() {
		try {
			return parent.getGenericOntologyHelper().
			        getStringAnnotation(individual, RegulatoryOntologyHelper.PROP_MARISKENTRY_TEXT);
		} catch (NoSuchPropertyException e) {
			throw new InvalidOWLOntologyException("Entry without text: "+individual.toString());
		}
	}

	@Override
	public String getType() {
		String type = "";
		if(this.toString().contains("Binding")){ type = "Binding"; }
		else if(this.toString().contains("Comment")){ type = "Comment"; }
		return type;		
	}
	
	@Override
	public String getTitle(){
		return getNumber();
	}
}
