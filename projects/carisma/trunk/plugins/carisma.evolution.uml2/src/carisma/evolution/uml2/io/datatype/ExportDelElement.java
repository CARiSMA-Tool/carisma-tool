package carisma.evolution.uml2.io.datatype;

import carisma.evolution.DelElement;


public class ExportDelElement extends ExportDeltaElement{
	
	private String string = null;
	
	public ExportDelElement(DelElement delEle) {
		string = delEle.toString();
	}
	

	public String toString() {
		if (string != null) {
			return string;
		}
		return "- DELETEs: " + this.getTarget().getType() + " '" + this.getTarget().getType() + "'";
	}
}