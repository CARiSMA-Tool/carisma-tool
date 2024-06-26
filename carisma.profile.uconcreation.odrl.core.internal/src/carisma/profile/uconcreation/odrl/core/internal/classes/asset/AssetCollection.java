package carisma.profile.uconcreation.odrl.core.internal.classes.asset;

import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;

public interface AssetCollection extends Asset {
	
	public ConstraintInterface getRefinement();
	public void setRefinement(ConstraintInterface refinement);
	public String getSource();
	public void setSource(String source);
	
	
}
