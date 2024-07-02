package carisma.check.policycreation.profileimpl.core.action;

import java.util.Set;

import org.json.JSONObject;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.check.policycreation.profileimpl.core.constraint.ConstraintInterfaceImpl;

public abstract class ActionImpl extends ODRLClassImpl {
	ConstraintInterfaceImpl refinement;

	
	public ConstraintInterfaceImpl getRefinement() {
		return refinement;
	}
	public void setRefinement(ConstraintInterfaceImpl refinement) {
		this.refinement = refinement;
	}



	@Override
	public void fillJson(JSONObject newJson, Set<ODRLClassImpl> circlePreventionSet, UMLModelConverter handler) {
		super.fillJson(newJson, circlePreventionSet, handler);
		newJson.put(refinementName, false);
		
	}


	

}
