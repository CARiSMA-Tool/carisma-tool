package carisma.regulatory.ontology;


 
/**
 * Represents a Marisk Entry in the ontology.
 * @author dbuerger
 *
 */
public interface MariskEntry extends MariskRule{

	/**
	 * the number of this binding.
	 * @return the number
	 */
	public String getNumber();
	
	/**
	 * the text of this binding.
	 * @return the content
	 */
	public String getText();
	
	/**
	 * returns the type of the entry. Whether it is a bindign or a comment entry.
	 * @return the type of the entry
	 */
	public String getType();
	
}
