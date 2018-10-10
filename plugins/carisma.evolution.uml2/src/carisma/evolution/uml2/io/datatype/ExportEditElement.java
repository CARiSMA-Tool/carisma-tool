package carisma.evolution.uml2.io.datatype;

import java.util.Map;

import carisma.evolution.EditElement;


/** Class used to Export EditElements.
 * @author bberghoff
 *
 */
public class ExportEditElement extends ExportDeltaElement {
	
    /**
     * Key value pair describing the values of an EditElement.
     */
	private Map<String, Object> values;
	/**
	 * Used by toString().
	 */
	private String string = null;

	/** Constructor which creates an ExportEditElement by a given editElement.
	 * Sets the values and the String used by the toString() method.
	 * @param editElement Element which is transformed.
	 */
	public ExportEditElement(final EditElement editElement) { 
	    this.values = ExporterUtility.getValuesWithStringNull(editElement.getValues());
		this.string = editElement.toString();
	}
	
	/** Getter for the values.
	 * 
	 * @return key value pair.
	 */
	public final Map<String, Object> getValues() {
		return this.values;
	}

	/** Set the key value pair.
	 * @param values values to set.
	 */
	public final void setValues(final Map<String, Object> values) {
		this.values.clear();
		this.values.putAll(values);
	}
	
	/** String output of this instance.
	 * Normally uses the toString() method of the EditElement with which the constructor was called.
	 * @return the output String.
	 */
	@Override
	public final String toString() {
		if (this.string != null) {
			return this.string;
		}
		StringBuffer output = new StringBuffer("- EDIT: ");
		output.append(this.getTarget().getType());
		output.append(" '");
		output.append(this.getTarget().getName());
		output.append("'");
		
		for (String key : this.getValues().keySet()) {
		    output.append("\n attribute '");
		    output.append(key);
		    output.append("' set to '");
		    output.append(this.getValues().get(key));
		}
		return output.toString();
	}

}
