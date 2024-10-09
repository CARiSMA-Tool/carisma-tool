package carisma.check.uconpolicycreation.profileclasses.core.action;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.constraints.ConstraintInterface;

public abstract class Action extends ODRLClass {
	/**
	 * Refines this Action.
	 */
	ConstraintInterface refinement;

	
	public ConstraintInterface getRefinement() {
		return refinement;
	}
	public void setRefinement(ConstraintInterface refinement) {
		this.refinement = refinement;
	}

	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		if (refinement == null) {
			return getClassTerm();
		} else {
			Map<String,Object> valueEntry= new HashMap<>();
			valueEntry.put(getIdKeyword(), handler.applyContext(getClassTerm()));//ODRL requires name-Prefix here, reached through context application
			map.put("rdf:value", valueEntry);
			List<Map<String,Object>> wrapperList = new LinkedList<>();
			wrapperList.add(map);
			return wrapperList;
		}
	}

}
