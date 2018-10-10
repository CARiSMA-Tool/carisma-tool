package carisma.evolution.uml2.io.datatype;

import carisma.evolution.DelElement;


public class ExportDelElement extends ExportDeltaElement{
	
	private String string = null;
	
	public ExportDelElement(DelElement delEle) {
		this.string = delEle.toString();
	}
	

	@Override
	public String toString() {
		if (this.string != null) {
			return this.string;
		}
		return "- DELETEs: " + this.getTarget().getType() + " '" + this.getTarget().getType() + "'";
	}
}