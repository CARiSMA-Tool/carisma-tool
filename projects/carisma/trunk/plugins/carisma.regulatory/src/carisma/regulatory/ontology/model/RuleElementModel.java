package carisma.regulatory.ontology.model;

/**
 * @author dbuerger
 * a model to deal with ruleElements.
 *
 */
public class RuleElementModel {
	
	private int startIndex = 0;
	private int endIndex = 0;
	private String representation = "";
	private String name = "";
	private String type = "";
	
	public RuleElementModel(final int start, final int end, final String representation, final String name, final String type) {
		this.setStartIndex(start);
		this.setEndIndex(end);
		this.setRepresentation(representation);
		this.setName(name);
		this.setType(type);
	}

	/**
	 * @return the startIndex
	 */
	public int getStartIndex() {
		return startIndex;
	}

	/**
	 * @param startIndex the startIndex to set
	 */
	private void setStartIndex(int startIndex) {
		this.startIndex = startIndex;
	}

	/**
	 * @return the endIndex
	 */
	public int getEndIndex() {
		return endIndex;
	}

	/**
	 * @param endIndex the endIndex to set
	 */
	private void setEndIndex(int endIndex) {
		this.endIndex = endIndex;
	}

	/**
	 * @return the representation
	 */
	public String getRepresentation() {
		return representation;
	}

	/**
	 * @param representation the representation to set
	 */
	private void setRepresentation(String representation) {
		this.representation = representation;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name the name to set
	 */
	private void setName(String name) {
		this.name = name;
	}

	/**
	 * @return the type
	 */
	public String getType() {
		return type;
	}

	/**
	 * @param type the type to set
	 */
	private void setType(String type) {
		this.type = type;
	}

}
