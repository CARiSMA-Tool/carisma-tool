package carisma.profile.umlsec;

import java.util.ArrayList;

import org.eclipse.emf.common.util.EList;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Parameter;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Type;

public class SignatureHelper {


	public static String getSignature(Operation operation) {
		StringBuffer signature = new StringBuffer(operation.getName());
		signature.append('(');
		EList<Parameter> ownedParameters = operation.getOwnedParameters();
		if(ownedParameters.size() > 0) {
			ArrayList<String> params = new ArrayList<String>(ownedParameters.size());
			for (Parameter p : ownedParameters) {
				if (p.equals(operation.getReturnResult())) {
					continue;
				}
				StringBuilder paramBuilder = new StringBuilder();
				paramBuilder.append(p.getName());
				paramBuilder.append(":");
				Type type = p.getType();
				if (type != null) {
					paramBuilder.append(p.getType().getName());
				}
				else {
					paramBuilder.append("void");
				}
				params.add(paramBuilder.toString());
			}
			signature.append(String.join(", ", params));
		}
		if (operation.getType() != null) {
			signature.append("):");
			signature.append(operation.getType().getName());
		} else {
			signature.append(')');
		}
		return signature.toString();
	}
	
	public static String getSignature(Property property) {
		StringBuilder propertyBuilder = new StringBuilder(property.getName());
		propertyBuilder.append(':');
		propertyBuilder.append(property.getType().getName());
		return propertyBuilder.toString();
	}
}
