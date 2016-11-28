package carisma.evolution.uml2.io.datatype;


public class ExportDeltaElement {
	private ExportExtTag target = null;
	
	public void setExt(ExportExtTag newTarget) { 
		this.target = newTarget;
	}
	
	public ExportExtTag getTarget() {
		return this.target;
	}
	
	@Override
	public String toString() {
		return this.target.getName();
	}
}
