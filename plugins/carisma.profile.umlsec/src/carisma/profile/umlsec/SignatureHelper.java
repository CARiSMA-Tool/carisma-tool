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

	public static String getQualifiedSignature(final Classifier classifier) {
		EObject container = classifier.eContainer();
		if (container instanceof Model) {
			return classifier.getName();
		}
		final Stack<String> names = new Stack<>();
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

		final StringBuilder builder = new StringBuilder();
		while (!names.isEmpty()) {
			builder.append(names.pop());
		}
		builder.append(classifier.getName());
		return builder.toString();
	}

	public static String getQualifiedSignature(final Operation operation) {
		final Element owner = operation.getOwner();
		if (owner instanceof Classifier) {
			final Classifier classifier = (Classifier) owner;
			return getQualifiedSignature(classifier).concat(".").concat(getSignature(operation));
		} else {
			throw new RuntimeException("unsupported owner: " + owner);
		}
	}

	public static String getQualifiedSignature(final Property property) {
		final Element owner = property.getOwner();
		if (owner instanceof Classifier) {
			final Classifier classifier = (Classifier) owner;
			return getQualifiedSignature(classifier).concat(getSignature(property));
		} else {
			throw new RuntimeException("unsupported owner: " + owner);
		}
	}

	public static String getSignature(final Operation operation) {
		final StringBuffer signature = new StringBuffer(operation.getName());
		signature.append('(');
		final EList<Parameter> ownedParameters = operation.getOwnedParameters();
		if (ownedParameters.size() > 0) {
			final ArrayList<String> params = new ArrayList<>(ownedParameters.size());
			for (final Parameter p : ownedParameters) {
				if (p.equals(operation.getReturnResult())) {
					continue;
				}
				final StringBuilder paramBuilder = new StringBuilder();
				final Type type = p.getType();
				if (type != null) {
					paramBuilder.append(p.getType().getName());
				} else {
					// TODO find meaningful fallback type for parameters without type (=broken specification in UML model)
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
			signature.append("):void");
		}
		return signature.toString();
	}

	public static String getSignature(final Property property) {
		final StringBuilder propertyBuilder = new StringBuilder(property.getName());
		final Type type = property.getType();
		if (type != null) {
			propertyBuilder.append(':');
			propertyBuilder.append(type.getName());
		}
		return propertyBuilder.toString();
	}
}
