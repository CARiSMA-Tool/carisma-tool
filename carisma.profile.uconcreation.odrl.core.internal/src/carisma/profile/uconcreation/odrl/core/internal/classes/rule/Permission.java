package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import java.util.Set;

public class Permission extends Rule {
	Set<Duty> duties;

	public Set<Duty> getDuties() {
		return duties;
	}

	public void setDuties(Set<Duty> duties) {
		this.duties = duties;
	}
	
	
}
