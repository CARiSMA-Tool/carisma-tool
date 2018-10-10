package carisma.regulatory.ontology.utils;

public class OntologyHelperException extends RuntimeException {
	private static final long serialVersionUID = 908005923250581986L;

	OntologyHelperException(Exception e, String message) {
		super(message, e);
	}
}

