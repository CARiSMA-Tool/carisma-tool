package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import java.util.List;

public class Permission extends Rule {
	List<Duty> duties;

	public List<Duty> getDuties() {
		return duties;
	}

	public void setDuties(List<Duty> duties) {
		this.duties = duties;
	}
	
	
}
