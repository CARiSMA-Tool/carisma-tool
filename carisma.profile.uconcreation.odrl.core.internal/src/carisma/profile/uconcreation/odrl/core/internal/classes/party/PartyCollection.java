package carisma.profile.uconcreation.odrl.core.internal.classes.party;

import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;

public interface PartyCollection extends Party {

	public ConstraintInterface getRefinement();
	public void setRefinement(ConstraintInterface refinement);
	public String getSource();
	public void setSource(String source);
	
	
}
