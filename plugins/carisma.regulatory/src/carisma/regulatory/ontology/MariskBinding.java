package carisma.regulatory.ontology;

/**
 * Represents a Marisk Binding in the ontology.
 * @author dbuerger
 *
 */
public interface MariskBinding extends MariskEntry{
	
	/**
	 * the comment of the binding
	 * @return
	 */
	public MariskComment getComment();

}
