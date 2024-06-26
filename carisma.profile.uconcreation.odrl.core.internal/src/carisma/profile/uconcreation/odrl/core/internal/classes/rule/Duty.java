package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Consequence;

public interface Duty extends Rule {

	public Consequence getConsequences();

	public void setConsequences(Consequence consequences);
	
}
