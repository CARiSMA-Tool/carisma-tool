package carisma.xutils.regulatory.ui.model;
// TODO: Auto-generated Javadoc

/**
 * This class should be used to arrange the ruleelements in the following methods.
 *
 * @author DB
 */
public enum RULEELEMENTS {		// TODO_later replace with values which come directly from the ontology
	
	/** The Activity. */
	Activity(0), 
	
	/** The Artifact. */
	Artifact(1),
	
	/** The Process. */
	Process(2), 
	
	/** The Property. */
	Property(3), 
	
	/** The Role. */
	Role(4); 
	
	
	
	/** The value. */
	private int value;
	
	/**
	 * Instantiates a new ruleelements.
	 *
	 * @param value the value
	 */
	private RULEELEMENTS(int value) {
		this.value = value;
	}
	
	/**
	 * Gets the value.
	 *
	 * @return the value
	 */
	public int getValue() {
		return this.value;
	}
	
}
