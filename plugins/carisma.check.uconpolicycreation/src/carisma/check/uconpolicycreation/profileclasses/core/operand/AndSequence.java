package carisma.check.uconpolicycreation.profileclasses.core.operand;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import carisma.check.uconpolicycreation.profileclasses.ODRLClass;

public class AndSequence extends Operand {
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClass> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		Map<String, Object> wrapper = new HashMap<String, Object>();//TODO: possibly extend to other operands? the example uses it for xone too but information model only specifies need for this class
		wrapper.put("@list", map.get(getFieldTerm("constraints")));
		return wrapper;
	}

}
