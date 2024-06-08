package carisma.profile.uconcreation.odrl.core.internal.classes.asset;

import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;

public class AssetCollection extends Asset {
	ConstraintInterface refinement;
	String source;
	
	String refinementName;
	String sourceName;
	
	
	public ConstraintInterface getRefinement() {
		return refinement;
	}
	public void setRefinement(ConstraintInterface refinement) {
		this.refinement = refinement;
	}
	public String getSource() {
		return source;
	}
	public void setSource(String source) {
		this.source = source;
	}
	
	
}
