package carisma.evolution.uml2.io.datatype;


public class ExportExtTagTaggedValue extends ExportExtTagStereotype {
	
	/**
	 * Name of the stereotype to which the TaggedValue belongs to.
	 */
	private String stereotype;

	
	/** The Target is a Tagged Value.
	 * 
	 * @param type Type of the Target
	 * @param name Name of the Target
	 * @param xmiID Xmi ID of the containing Stereotype
	 * @param extendetElement Element to which the containing Stereotype is applied to
	 * @param profile to which the containing Stereotype belongs to
	 * @param stereotype Stereotype to which the Tagged Value belongs to
	 */
	public ExportExtTagTaggedValue(String type, String name, String xmiID, String extendetElement, String profile, String stereotype) {
		super(type, name, xmiID, extendetElement, profile);
		this.stereotype = stereotype;
	}

	public String getStereotype() {
		return this.stereotype;
	}

	public void setStereotype(String stereotype) {
		this.stereotype = stereotype;
	}
}
