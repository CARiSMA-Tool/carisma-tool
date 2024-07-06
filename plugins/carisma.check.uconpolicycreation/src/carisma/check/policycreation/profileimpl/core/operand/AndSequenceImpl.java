package carisma.check.policycreation.profileimpl.core.operand;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;

public class AndSequenceImpl extends OperandImpl {
	
	@Override
	public Object fillMapIndividual(Map<String,Object> map, Set<ODRLClassImpl> circlePreventionSet) throws NoSuchFieldException, SecurityException {
		Map<String, Object> wrapper = new HashMap<String, Object>();//TODO: possibly extend to other operands? the example uses it for xone too
		wrapper.put("@list", map.get(getFieldTerm("constraints")));
		return wrapper;
	}

}
