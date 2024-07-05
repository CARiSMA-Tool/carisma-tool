package carisma.check.policycreation.profileimpl.core.action;

import java.util.HashMap;
import java.util.Map;
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
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		if (refinement == null) {
			return gatClassTerm();
		} else {
			Map<String,String> valueEntry= new HashMap<>();
			valueEntry.put(gatIdKeyword(), gatClassTerm());//TODO: expand the classTerm with its namespace identifier prefix (extract from the context file)
			map.put("rdf:value", valueEntry);
		}
		return null;
	}

}
