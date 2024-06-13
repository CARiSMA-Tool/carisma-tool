package carisma.profile.uconcreation.odrl.core.internal.classes;

import java.util.Set;
import java.util.TreeSet;

public abstract class ODRLClass {
	public static final int stringIndent = 4;
	protected Set<ODRLClass> referredBy;
	

	
	

	public String getType() {//for the automatic JSON-Conversion currently used for testing
		return this.getClass().getSimpleName();
	}
}
