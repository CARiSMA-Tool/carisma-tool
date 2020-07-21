package carisma.profile.umlsec;

import java.util.ArrayList;
import java.util.Stack;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Parameter;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Type;

public class SignatureHelper {

	public static String getQualifiedSignature(Classifier classifier) {
		EObject container = classifier.eContainer();
		if (container instanceof Model) {
			return classifier.getName();
		}
		Stack<String> names = new Stack<>();
		do {
			if (container instanceof Package) {
				names.add(".");
				names.add(((Package) container).getName());
			} else if (container instanceof Classifier) {
				names.add("$");
				names.add(((Classifier) container).getName());
			} else {
				throw new RuntimeException("Unsupported container: " + container);
			}
			container = container.eContainer();
		} while (!(container instanceof Model));

		StringBuilder builder = new StringBuilder();
		while (!names.isEmpty()) {
			builder.append(names.pop());
		}
		builder.append(classifier.getName());
		return builder.toString();
	}

	public static String getQualifiedSignature(Operation operation) {
		Element owner = operation.getOwner();
		if (owner instanceof Classifier) {
			Classifier classifier = (Classifier) owner;
			return getQualifiedSignature(classifier).concat(getSignature(operation));
		} else {
			throw new RuntimeException("unsupported owner: " + owner);
		}
	}
	
	public static String getQualifiedSignature(Property property) {
		Element owner = property.getOwner();
		if (owner instanceof Classifier) {
			Classifier classifier = (Classifier) owner;
			return getQualifiedSignature(classifier).concat(getSignature(property));
		} else {
			throw new RuntimeException("unsupported owner: " + owner);
		}
	}

	public static String getSignature(Operation operation) {
		StringBuffer signature = new StringBuffer(operation.getName());
		signature.append('(');
		EList<Parameter> ownedParameters = operation.getOwnedParameters();
		if (ownedParameters.size() > 0) {
			ArrayList<String> params = new ArrayList<String>(ownedParameters.size());
			for (Parameter p : ownedParameters) {
				if (p.equals(operation.getReturnResult())) {
					continue;
				}
				StringBuilder paramBuilder = new StringBuilder();
//				paramBuilder.append(p.getName());
//				paramBuilder.append(":");
				Type type = p.getType();
				if (type != null) {
					paramBuilder.append(p.getType().getName());
				} else {
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
		Type type = property.getType();
		if (type != null) {
			propertyBuilder.append(type.getName());
		}
		return propertyBuilder.toString();
	}
}
