package carisma.profile.uconcreation.odrl.core.internal.classes;

import java.util.Set;
import java.util.TreeSet;
import org.eclipse.emf.ecore.ENamedElement;

public abstract class ODRLClass {
	protected Set<ODRLClass> referredBy;
	public ENamedElement containingUMLElement;
	public ODRLClass directParent;//maybe
	

	
	

	public String getType() {//for the automatic JSON-Conversion currently used for testing
		return this.getClass().getSimpleName();
	}
	
	
}
