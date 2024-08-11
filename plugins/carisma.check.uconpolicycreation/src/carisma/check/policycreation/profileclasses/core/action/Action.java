package carisma.check.policycreation.profileclasses.core.action;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileclasses.ODRLClass;
import carisma.check.policycreation.profileclasses.core.constraints.ConstraintInterface;

public abstract class Action extends ODRLClass {
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
			return gatClassTerm();
		} else {
			Map<String,String> valueEntry= new HashMap<>();
			valueEntry.put(gatIdKeyword(), gatClassTerm());//TODO: expand the classTerm with its namespace identifier prefix (extract from the context file)
			map.put("rdf:value", valueEntry);
		}
		return null;
	}

}
