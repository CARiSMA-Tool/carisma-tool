package carisma.regulatory.ontology;

import java.util.List;

/**
 * Represents a Paragraph of a law in the ontology.
 * @author wenzel
 *
 */
public interface Paragraph extends LawRule {

	/**
	 * The law to which this paragraph belongs.
	 * @return
	 */
	public Law getLaw();
	
	/**
	 * The number of this paragraph.
	 * @return
	 */
	public String getNumber();
	
	
	/**
	 * The contained sections of this paragraph.
	 * @return
	 */
	public List<Section> getSections();
	
}
