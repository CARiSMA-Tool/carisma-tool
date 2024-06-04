package carisma.profile.uconcreation.odrl.core.internal.classes;

import java.util.Set;
import java.util.TreeSet;

public abstract class ODRLClass {
	
	protected Set<ODRLClass> referredBy;
	
	public Set<String> getOdrlAttributes() {
		return new TreeSet<>();
	};
	
	
	
}
