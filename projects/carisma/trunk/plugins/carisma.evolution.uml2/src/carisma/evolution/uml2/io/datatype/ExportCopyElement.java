package carisma.evolution.uml2.io.datatype;

import java.util.HashMap;
import java.util.Map;

import carisma.evolution.CopyElement;


/** Class used to Export CopyElements.
 * @author bberghoff
 *
 */
public class ExportCopyElement extends ExportAdditiveElement {
	
    /**
     * Target of this Element. 
     */
	private ExportExtTag targetOwner;
    /**
     * Key value pair describing the changed values of an CopyElement.
     */
	private Map<String, Object> changedValues = null;

    /**
     * Used by toString().
     */
	private String string = null;
	
	/** Constructor which creates an ExportCopyElement by a given CopyElement.
     * Sets the changed values and the String used by the toString() method.
     * @param cpEle Element which is transformed.
     */
	public ExportCopyElement(final CopyElement cpEle) {
		changedValues = new HashMap<String, Object>(
				cpEle.getChangedValues());
		string = cpEle.toString();
	}

	/** Set the target.
	 * 
     * @param newTargetOwner target to set.
     */
	public final void setTargetOwner(final ExportExtTag newTargetOwner) {
		targetOwner = newTargetOwner;
	}
	
	/** Getter for the changed values.
     * 
     * @return key value pair.
     */
	public final Map<String, Object> getChangedValues() {
		return changedValues;
	}
	
	/** Getter for the target.
     * 
     * @return the target saved as an ExportExtTag.
     */
	public final ExportExtTag getTargetOwner() {
		return targetOwner;
	}
	
	/** String output of this instance.
     * Normally uses the toString() method of the EditElement with which the constructor was called.
     * @return the output String.
     */
	public final String toString() {
		if (string != null) {
			return string;
		}
		StringBuffer output = new StringBuffer("- COPY: ");
		output.append(this.getTarget().getType());
		output.append(" '");
		output.append(this.getTarget().getName());
		output.append("'. To: "); 
		output.append(this.getTargetOwner().getType());
		output.append(" '");
		output.append(this.getTargetOwner().getName());
		output.append("'");
		for (String key : this.getChangedValues().keySet()) {
		    output.append(System.getProperty("line.seperator"));
		    output.append("Changed value");
		    output.append(key);
		    output.append(" TO: ");
		    output.append(this.getChangedValues().get(key));
		}
		return output.toString();
	}
}
