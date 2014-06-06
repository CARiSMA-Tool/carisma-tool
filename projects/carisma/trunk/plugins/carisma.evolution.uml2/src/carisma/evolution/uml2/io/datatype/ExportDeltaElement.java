package carisma.evolution.uml2.io.datatype;


public class ExportDeltaElement {
	private ExportExtTag target = null;
	
	public void setExt(ExportExtTag newTarget) { 
		target = newTarget;
	}
	
	public ExportExtTag getTarget() {
		return target;
	}
	
	public String toString() {
		return target.getName();
	}
}
