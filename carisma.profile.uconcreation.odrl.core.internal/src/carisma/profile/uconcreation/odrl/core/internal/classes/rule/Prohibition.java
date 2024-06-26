package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Remedy;

public interface Prohibition extends Rule {
	
	public Remedy getRemedy();

	public void setRemedy(Remedy remedy);
	
	
}
