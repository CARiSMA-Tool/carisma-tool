package carisma.evolution.uml2.io.datatype;

public class ExportChangeConstraint {
	private String type;
	private String referencedChange;

	public ExportChangeConstraint() {
		type = "";
		referencedChange = "";
	}
	
	public ExportChangeConstraint(final String newType,final String newReferencedChange) {
		type = newType;
		referencedChange = newReferencedChange;
	}
	
	public String getType() {
		return type;
	}
	
	public String getReferencedChange() {
		return referencedChange;
	}
}
