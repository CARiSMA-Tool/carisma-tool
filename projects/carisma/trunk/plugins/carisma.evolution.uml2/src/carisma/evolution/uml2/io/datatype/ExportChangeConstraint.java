package carisma.evolution.uml2.io.datatype;

public class ExportChangeConstraint {
	private String type;
	private String referencedChange;

	public ExportChangeConstraint() {
		this.type = "";
		this.referencedChange = "";
	}
	
	public ExportChangeConstraint(final String newType,final String newReferencedChange) {
		this.type = newType;
		this.referencedChange = newReferencedChange;
	}
	
	public String getType() {
		return this.type;
	}
	
	public String getReferencedChange() {
		return this.referencedChange;
	}
}
