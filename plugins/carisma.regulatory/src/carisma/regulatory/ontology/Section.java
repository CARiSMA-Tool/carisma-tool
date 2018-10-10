package carisma.regulatory.ontology;

/**
 * Represents a Section of a law paragraph in the ontology.
 * @author wenzel
 *
 */
public interface Section extends LawRule {

	/**
	 * The paragraph that contains this section.
	 * @return
	 */
	public Paragraph getParagraph();
	
	/**
	 * The number of this section.
	 * @return
	 */
	public String getNumber();

	/**
	 * The content of this section, i.e. the text. 
	 * @return
	 */
	public String getContent();
	
}
