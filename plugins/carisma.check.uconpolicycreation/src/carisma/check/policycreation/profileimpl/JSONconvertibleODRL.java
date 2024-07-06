package carisma.check.policycreation.profileimpl;

import java.util.Set;

import org.json.JSONObject;

import carisma.check.policycreation.UMLModelConverter;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;

public interface JSONconvertibleODRL {
	public JSONObject convertToJson(Set<ODRLClassImpl> circlePreventionSet, UMLModelConverter handler);
}
