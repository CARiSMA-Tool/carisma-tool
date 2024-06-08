package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Consequence;

public class Duty extends Rule {
	Consequence consequences;
	
	String consequencesName;

	public Consequence getConsequences() {
		return consequences;
	}

	public void setConsequences(Consequence consequences) {
		this.consequences = consequences;
	}
	
	
}
