package carisma.profile.uconcreation.odrl.core.internal.classes.rule;

import java.util.List;

public interface Permission extends Rule {
	
	public List<Duty> getDuties();

	public void setDuties(List<Duty> duties);
	
	
}
