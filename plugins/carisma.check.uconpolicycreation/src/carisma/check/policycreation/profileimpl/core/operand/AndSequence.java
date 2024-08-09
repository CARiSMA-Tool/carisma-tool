package carisma.check.policycreation.profileimpl.core.operand;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileimpl.core.ODRLClass;

public class AndSequence extends Operand {
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		Map<String, Object> wrapper = new HashMap<String, Object>();//TODO: possibly extend to other operands? the example uses it for xone too
		wrapper.put("@list", map.get(getFieldTerm("constraints")));
		return wrapper;
	}

}
