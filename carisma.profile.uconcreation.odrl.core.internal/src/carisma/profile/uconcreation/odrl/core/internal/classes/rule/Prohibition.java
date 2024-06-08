package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Remedy;

public class Prohibition extends Rule {
	Remedy remedy;
	
	String remedyName;

	public Remedy getRemedy() {
		return remedy;
	}

	public void setRemedy(Remedy remedy) {
		this.remedy = remedy;
	}
	
	
}
