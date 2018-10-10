package carisma.evolution.uml2.io.datatype;

import java.util.ArrayList;
import java.util.List;

import carisma.evolution.AddElement;
import carisma.evolution.SubstElement;


/** Class used to Export SubstElements.
 * @author bberghoff
 *
 */
public class ExportSubstElement extends ExportAdditiveElement {
	
//	private List<ExportExtTag> accompanyingDeletions = null;

    /**
     * All components which belong to this SubstElement.
     */
	private List<ExportAddElement> components = null;

    /**
     * Used by toString().
     */
	private String string = null;

    /** Constructor which creates an ExportSubstElement by a given SubstElement.
     * Sets the components and the String used by the local toString() method to subE.toString().
     * @param subE Element which is transformed.
     */
	public ExportSubstElement(final SubstElement subE) {
		this.components = new ArrayList<>();
		if (subE.getAllAddedElements() != null) {
    		for (AddElement e : subE.getAllAddedElements()) {
    			this.components.add(new ExportAddElement(e));
    		}
		}
		this.string = subE.toString();
//		if(subE.getAccompanyingDeletions() != null){
//			accompanyingDeletions = new ArrayList<ExportExtTag>();
//			for(EObject accompanyingDeletion : subE.getAccompanyingDeletions()) { 
//				
//			}
//			accompanyingDeletions.addAll(subE.getAccompanyingDeletions());
//		}
	}	

//	public List<EObject> getAccompanyingDeletions() {
//		return accompanyingDeletions;
//	}


    /** Getter for the components.
     * 
     * @return List with all components.
     */
	public final List<ExportAddElement> getComponents() {
		return this.components;
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
		StringBuffer output = new StringBuffer("- REPLACE");
		output.append(this.getTarget().getType());
		output.append(" '");
		output.append(this.getTarget().getName());
		output.append("' with");
		for (ExportAddElement add : this.getComponents()) {
		    output.append(System.getProperty("line.seperator"));
		    output.append("    - ");
		    output.append(add.getType());
			for (String key : add.getValues().keySet()) {
			    output.append(" ");
		        output.append(key);
		        output.append(" ");
		        output.append(add.getValues().get(key));
			}
		} 
		return output.toString();
	}
}
