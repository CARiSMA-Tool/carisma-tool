package carisma.regulatory.ontology;

import java.util.Collection;

/**
 * Represents a BSI element (Baustein) in the ontology.
 * @author wenzel
 *
 */
public interface BSIElement extends BSIRule {

	/**
	 * The threats of this element.
	 * @return
	 */
	public Collection<BSIThreat> getThreats();
	
	/**
	 * The measures of the elements.
	 * @return
	 */
	public Collection<BSIMeasure> getMeasures();
	
}
