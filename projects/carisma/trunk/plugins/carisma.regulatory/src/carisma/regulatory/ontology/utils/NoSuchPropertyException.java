package carisma.regulatory.ontology.utils;

import org.semanticweb.owlapi.model.OWLEntity;

/**
 * Thrown if a property is requested that does not exist.
 * @author wenzel
 *
 */
public class NoSuchPropertyException extends Exception {
	
	private static final long serialVersionUID = 908005923250581986L;

	NoSuchPropertyException(String property, OWLEntity entity) {
		super("No such property '" + property + "' for: " + entity);
	}

}
