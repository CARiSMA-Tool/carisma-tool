package carisma.regulatory.ontology;

import java.util.List;

/**
 * Represents a Law individual in the ontology.
 * @author wenzel
 *
 */
public interface Law extends LawRule {

	/**
	 * The paragraphs of this law. 
	 * @return
	 */
	public List<Paragraph> getParagraphs();
	
}
