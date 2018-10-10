package carisma.evolution.uml2.io.datatype;


public class ExportExtTagStereotype extends ExportExtTag{

	private String extendedElement;
	
	private String profile;
	
	
	/** The Target is a Stereotype
	 * 
	 * @param type Type of the Target
	 * @param name Name of the Target
	 * @param xmiID Xmi ID of the Target
	 * @param extendedElement Element to which the Target is applied to
	 * @param profile Profile to which the Target belongs to
	 */
	public ExportExtTagStereotype(String type, String name, String xmiID, String extendedElement, String profile) {
		super(type, name, xmiID);
		this.extendedElement = extendedElement;
		this.profile = profile;
	}

	public String getExtendedElement() {
		return this.extendedElement;
	}

	public void setExtendedElement(String extendedElement) {
		this.extendedElement = extendedElement;
	}
	
	public String getProfile(){
		return this.profile;
	}
}
