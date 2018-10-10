package carisma.evolution.uml2.io.datatype;

public abstract class ExportExtTag {
	private String type;
	private String name;
	private String xmiID;
	
	/** 
	 * 
	 * @param type Type of the Target
	 * @param name Name of the Target
	 * @param xmiID Xmi ID of the Target
	 */
	public ExportExtTag(String type, String name, String xmiID){
		this.type = type;
		this.name = name;
		this.xmiID = xmiID;
	}

	public String getType() {
		return this.type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getXmiID() {
		return this.xmiID;
	}

	public void setXmiID(String xmiID) {
		this.xmiID = xmiID;
	}
}
