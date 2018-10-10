package carisma.regulatory.ontology.owl;

/**
 * Thrown if the ontology is not conform to the regulatory schema, e.g. a rule misses a name. 
 * @author wenzel
 *
 */
public class InvalidOWLOntologyException extends RuntimeException {

	private static final long serialVersionUID = -179435639643758914L;

	public InvalidOWLOntologyException(String message, Throwable exception) {
		super(message, exception);
	}

	public InvalidOWLOntologyException(String message) {
		super(message);
	}
	
	

}
