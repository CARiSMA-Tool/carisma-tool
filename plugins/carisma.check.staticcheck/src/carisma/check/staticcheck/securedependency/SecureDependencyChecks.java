/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.check.staticcheck.securedependency;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Deployment;
import org.eclipse.uml2.uml.DirectedRelationship;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Generalization;
import org.eclipse.uml2.uml.Interface;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Realization;
import org.eclipse.uml2.uml.Relationship;
import org.eclipse.uml2.uml.UMLPackage;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.SignatureHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;
import carisma.profile.umlsec.call;
import carisma.profile.umlsec.critical;
import carisma.profile.umlsec.high;
import carisma.profile.umlsec.integrity;
import carisma.profile.umlsec.privacy;
import carisma.profile.umlsec.secrecy;
import carisma.profile.umlsec.send;

/**
 * Functions to process UMLsec properties.
 *
 * @author Sven Wenzel
 *
 */
public final class SecureDependencyChecks {

	private final List<SecureDependencyViolation> secureDependencyViolations;
	private AnalysisHost analysisHost;

	/**
	 * Private constructor. UMLsec will never be initialized.
	 */
	public SecureDependencyChecks(final AnalysisHost host) {
		if (host != null) {
			this.analysisHost = host;
		} else {
			this.analysisHost = new DummyHost(true);
		}

		this.secureDependencyViolations = new ArrayList<>();
	}

	public List<SecureDependencyViolation> getViolations() {
		return Collections.unmodifiableList(this.secureDependencyViolations);
	}

	/**
	 * Checks whether a model fulfills the secure dependency requirement.
	 *
	 * @param model
	 * @return
	 */
	public int checkSecureDependency(final Package model) {
		final List<Dependency> dependenciesToCheck = new ArrayList<>(
				UMLHelper.getAllElementsOfType(model, Dependency.class));
		for (final Dependency dep : dependenciesToCheck) {
			analyzeDependency(dep);
		}
		this.analysisHost.appendLineToReport(
				"\n------------------------------------------------------------------------------------");
		this.analysisHost
		.appendLineToReport("The analysis detected " + this.secureDependencyViolations.size() + " errors.");
		this.analysisHost.appendLineToReport(
				"------------------------------------------------------------------------------------");
		this.analysisHost.appendLineToReport(
				"------------------------------------------------------------------------------------\n");
		return this.secureDependencyViolations.size();
	}

	/**
	 * Checks the dependency.
	 *
	 * @param dep
	 */
	public void analyzeDependency(final Dependency dep) {
		if (dep instanceof Deployment) {
			return;
		}
		final List<NamedElement> clients = dep.getClients();
		final List<NamedElement> suppliers = dep.getSuppliers();
		for (final NamedElement c : clients) {
			for (final NamedElement s : suppliers) {
				if ((c instanceof Classifier) && (s instanceof Classifier)) {
					this.secureDependencyViolations.addAll(checkDependency(dep, (Classifier) c, (Classifier) s));
				}
			}
		}
	}

	public List<SecureDependencyViolation> checkDependency(final Dependency dependency, final Classifier client,
			final Classifier supplier) {
		this.analysisHost.appendLineToReport("\nProcessing dependency '" + dependency.getQualifiedName() + "' between '"
				+ client.getQualifiedName() + "' and '" + supplier.getQualifiedName() + "'");
		if (!isRelevantDependency(dependency)) {
			this.analysisHost.appendLineToReport("  - not in scope of <<secure dependency>> -> nothing to check!");
			return Collections.emptyList();
		}

		final List<SecureDependencyViolation> errors = new ArrayList<>();

		final List<String> freshClient = new ArrayList<>();
		final List<String> highClient = new ArrayList<>();
		final List<String> integrityClient = new ArrayList<>();
		final List<String> privacyClient = new ArrayList<>();
		final List<String> secrecyClient = new ArrayList<>();

		getCriticalTags(client, freshClient, highClient, integrityClient, privacyClient, secrecyClient);

		final ArrayList<String> freshRequiredClient = new ArrayList<>();
		getRequired(client, freshClient, new ArrayList<String>(), freshRequiredClient);

		final ArrayList<String> highRequiredClient = new ArrayList<>();
		getRequired(client, highClient, new ArrayList<String>(), highRequiredClient);

		final ArrayList<String> integrityRequiredClient = new ArrayList<>();
		getRequired(client, integrityClient, new ArrayList<String>(), integrityRequiredClient);

		final ArrayList<String> privacyRequiredClient = new ArrayList<>();
		getRequired(client, privacyClient, new ArrayList<String>(), privacyRequiredClient);

		final ArrayList<String> secrecyRequiredClient = new ArrayList<>();
		getRequired(client, secrecyClient, new ArrayList<String>(), secrecyRequiredClient);

		final List<Classifier> subSuppliers = getSubClassifiers(supplier);
		for (final Classifier subSupplier : subSuppliers) {
			if (subSupplier instanceof Interface) {
				continue;
			}
			final List<String> signaturesSupplier = getMemberSignatures(subSupplier);

			final List<String> freshSupplier = new ArrayList<>();
			final List<String> highSupplier = new ArrayList<>();
			final List<String> integritySupplier = new ArrayList<>();
			final List<String> privacySupplier = new ArrayList<>();
			final List<String> secrecySupplier = new ArrayList<>();

			getCriticalTags(subSupplier, freshSupplier, highSupplier, integritySupplier, privacySupplier,
					secrecySupplier);

			// errors.addAll(analyze(subSupplier, client, dependency,
			// freshSupplier, freshRequiredClient, signaturesSupplier,
			// fresh.class));
			errors.addAll(analyze(subSupplier, client, dependency, highSupplier, highRequiredClient, signaturesSupplier,
					high.class));
			errors.addAll(analyze(subSupplier, client, dependency, integritySupplier, integrityRequiredClient,
					signaturesSupplier, integrity.class));
			errors.addAll(analyze(subSupplier, client, dependency, privacySupplier, privacyRequiredClient,
					signaturesSupplier, privacy.class));
			errors.addAll(analyze(subSupplier, client, dependency, secrecySupplier, secrecyRequiredClient,
					signaturesSupplier, secrecy.class));
		}

		return errors;
	}

	private void getCriticalTags(final Classifier classifier, final List<String> fresh, final List<String> high,
			final List<String> integrity, final List<String> privacy, final List<String> secrecy) {
		for (final EObject stereotype : classifier.getStereotypeApplications()) {
			if (stereotype instanceof critical) {
				final critical critical = (critical) stereotype;
				fresh.addAll(critical.getFresh());
				high.addAll(critical.getHigh());
				integrity.addAll(critical.getIntegrity());
				privacy.addAll(critical.getPrivacy());
				secrecy.addAll(critical.getSecrecy());
			}
		}
	}

	private List<String> getRequired(final Classifier classifier, final Collection<String> criticalTags,
			final Collection<String> provided, final Collection<String> required) {
		final List<String> signatures = getMemberSignatures(classifier);

		for (final String tag : criticalTags) {
			final String[] names = tag.split("\\.");
			final int length = names.length;
			String signature = names[length - 1].replace(" ", "");
			if (!signature.contains(":")) {
				signature+=":void";
			}
			if (signatures.contains(signature)) {
				if (length == 1) {
					provided.add(signature);
				} else if (names[length - 2].equals(classifier.getName())) {
					if (length == 2) {
						provided.add(signature);
					} else {
						boolean equal = true;
						Element owner = classifier.getOwner();
						for (int i = length - 3; i >= 0; i--) {
							final String packageName = names[i];
							equal &= (owner != null) && (owner instanceof Package)
									&& ((NamedElement) owner).getName().equals(packageName);
							if (equal) {
								owner = owner.getOwner();
							} else {
								break;
							}
						}
						if (equal) {
							provided.add(signature);
						} else {
							required.add(signature);
						}
					}
				} else {
					required.add(signature);
				}
			} else {
				required.add(signature);
			}
		}

		return null;
	}

	public Collection<SecureDependencyViolation> analyze(final Classifier supplier, final Classifier client,
			final Dependency dependency, final Collection<String> taggedValueSupplier,
			final Collection<String> requiredClient, final Collection<String> signaturesSupplier,
			final Class<? extends EObject> criticalTag) {
		final ArrayList<SecureDependencyViolation> errors = new ArrayList<>();

		final ArrayList<String> providedSupplier = new ArrayList<>();
		final ArrayList<String> requiredSupplier = new ArrayList<>();
		getRequired(supplier, taggedValueSupplier, providedSupplier, requiredSupplier);
		final Collection<String> relevantRequired = intersection(requiredClient, signaturesSupplier);
		if ((relevantRequired.size() > 0) || (providedSupplier.size() > 0)) {
			final boolean requiredSubsetOfProvided = providedSupplier.containsAll(relevantRequired);
			if ((relevantRequired.size() != providedSupplier.size()) || !requiredSubsetOfProvided) {
				if (requiredSubsetOfProvided) {
					final List<String> set = new ArrayList<>(providedSupplier);
					(set).removeAll(relevantRequired);
					String description = '\"' + supplier.getName() + "\" provides {" + criticalTag.getSimpleName()
					+ "}";
					if (set.size() == 1) {
						description += " for the operation \"" + set.get(0) + "\"";
					} else {
						description += " for the operations ";
						if (set.size() == 2) {
							description += "\"" + set.get(0) + "\" and \"" + set.get(1) + "\" ";
						} else {
							int i = 0;
							for (; i < (set.size() - 1); i++) {
								description += "\"" + set.get(i) + "\", ";
							}
							description += "\"" + set.get(i) + "\" ";
						}
					}
					description += " for which \"" + client.getName() + "\" does not!";
					errors.add(new SecureDependencyViolation(description, dependency, client, supplier, set,
							criticalTag.getSimpleName(), client));
					this.analysisHost.appendLineToReport("    " + description);
				} else {
					final List<String> set = new ArrayList<>(relevantRequired);
					(set).removeAll(providedSupplier);
					String description = '\"' + client.getName() + "\" requires {" + criticalTag.getSimpleName() + "}";
					if (set.size() == 1) {
						description += " for the operation \"" + set.get(0) + "\"";
					} else {
						description += " for the operations ";
						if (set.size() == 2) {
							description += "\"" + set.get(0) + "\" and \"" + set.get(1) + "\" ";
						} else {
							int i = 0;
							for (; i < (set.size() - 1); i++) {
								description += "\"" + set.get(i) + "\", ";
							}
							description += "\"" + set.get(i) + "\" ";
						}
					}
					description += " for which \"" + supplier.getName() + "\" does not does not provide {"
							+ criticalTag.getSimpleName() + "}!";
					errors.add(new SecureDependencyViolation(description, dependency, client, supplier, set,
							criticalTag.getSimpleName(), supplier));
					this.analysisHost.appendLineToReport("    " + description);
				}
			}
			boolean hasStereotype = false;
			for (final EObject stereotype : dependency.getStereotypeApplications()) {
				hasStereotype |= criticalTag.isAssignableFrom(stereotype.getClass());
			}
			if (!hasStereotype) {
				errors.add(new SecureDependencyViolation(
						"Dependency misses stereotype <<" + criticalTag.getSimpleName() + ">>!", dependency, client,
						supplier, Collections.emptyList(), criticalTag.getSimpleName(), dependency));
				this.analysisHost.appendLineToReport(
						"    Dependency misses stereotype <<" + criticalTag.getSimpleName() + ">>!");
			}
		}

		return errors;
	}

	private Collection<String> intersection(final Collection<String> collectionA,
			final Collection<String> collectionB) {
		final ArrayList<String> intersection = new ArrayList<>();
		for (final String a : collectionA) {
			for (final String b : collectionB) {
				if (a.equals(b)) {
					intersection.add(a);
				}
			}
		}
		return intersection;
	}

	/**
	 * Retrieves all distinct tag values of the given tag name from the classifier
	 * and all of its subclassifiers.
	 *
	 * @param classifier
	 * @param tagName
	 * @return
	 */
	public static List<String> getAllDistinctTagValuesOfClassifierAndSubclasses(final Classifier classifier,
			final String tagName) {
		final List<String> distinctTagValues = new ArrayList<>();
		for (final Classifier subClassifier : getSubClassifiers(classifier)) {
			getDistinctTagValues(distinctTagValues, subClassifier, tagName);
		}
		return distinctTagValues;
	}

	/**
	 * Puts all distinct tag values of a given tag name at the <<critical>> app of a
	 * given classifier in a list.
	 *
	 * @param distinctTagValues - list of distinct tag values
	 * @param classifier        - the given classifier
	 * @param tagName           - the tag name to collect the values from
	 */
	private static void getDistinctTagValues(final List<String> distinctTagValues, final Classifier classifier,
			final String tagName) {
		final StereotypeApplication criticalApp = UMLsecUtil.getStereotypeApplication(classifier, UMLsec.CRITICAL);
		if (criticalApp != null) {
			final List<String> tagValues = UMLsecUtil.getStringValues(tagName, UMLsec.CRITICAL, classifier);
			for (String tagValue : tagValues) {
				if (!tagValue.contains("(") && !tagValue.contains(")")) {
					tagValue = tagValue + "()";
				}
				if (!distinctTagValues.contains(tagValue)) {
					distinctTagValues.add(tagValue);
				}
			}
		}
	}

	/**
	 * Returns a list of operation signatures of a given classifier. The signatures
	 * look like this: operationName(param1Name:param1Type,
	 * param2Name:param2Type,...):returnType
	 *
	 * @param classifier - the classifier whose operation signatures are to be
	 *                   collected
	 * @return - a list of operation signature strings
	 */
	public static List<String> getMemberSignatures(final Classifier classifier) {
		final List<String> signatures = new ArrayList<>();
		for (final Operation operation : classifier.getAllOperations()) {
			final String signature = SignatureHelper.getSignature(operation);
			signatures.add(signature);
		}
		for (final Property property : classifier.getAllAttributes()) {
			final String signature = SignatureHelper.getSignature(property);
			signatures.add(signature);
		}
		return signatures;
	}

	public static boolean isRelevantDependency(final Dependency dependency) {
		boolean isRelevant = UMLsecUtil.isInScopeOfStereotype(dependency, UMLsec.SECURE_DEPENDENCY);
		if (!isRelevant) {
			return false;
		}
		for (final EObject stereotype : dependency.getStereotypeApplications()) {
			if (stereotype instanceof call || stereotype instanceof send) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Retrieves a list of subclassifiers of the given classifier, including the
	 * classifier itself.
	 *
	 * @param classifier - the classifier to use as a basis
	 * @return - the list of subclassifiers
	 */
	public static List<Classifier> getSubClassifiers(final Classifier classifier) {
		final List<Classifier> subclassifiers = new ArrayList<>();
		getSubClassifiers(subclassifiers, classifier);
		return subclassifiers;
	}

	/**
	 * Retrieves all subclassifiers (specializations and interface realizations) of
	 * the given classifier and adds all of them, including the given classifier to
	 * the given list.
	 *
	 * @param subclassifiers - the list of subclassifiers
	 * @param classifier     - the given classifier
	 */
	private static void getSubClassifiers(final List<Classifier> subclassifiers, final Classifier classifier) {
		subclassifiers.add(classifier);
		List<DirectedRelationship> rels = classifier
				.getTargetDirectedRelationships(UMLPackage.eINSTANCE.getRealization());
		for (final Relationship rel : rels) {
			final Realization realization = (Realization) rel;
			for (final NamedElement sub : realization.getClients()) {
				if (sub instanceof Classifier) {
					getSubClassifiers(subclassifiers, (Classifier) sub);
				}
			}
		}
		rels = classifier.getTargetDirectedRelationships(UMLPackage.eINSTANCE.getGeneralization());
		for (final Relationship rel : rels) {
			final Generalization generalization = (Generalization) rel;
			getSubClassifiers(subclassifiers, generalization.getSpecific());
		}
	}

	public static List<Classifier> getSuperClassifiers(final Classifier classifier) {
		final ArrayList<Classifier> superclassifiers = new ArrayList<>();
		getSuperClassifiers(superclassifiers, classifier);
		return superclassifiers;
	}

	private static void getSuperClassifiers(final List<Classifier> superclassifiers, final Classifier classifier) {
		superclassifiers.add(classifier);
		List<DirectedRelationship> rels = classifier
				.getSourceDirectedRelationships(UMLPackage.eINSTANCE.getRealization());
		for (final Relationship rel : rels) {
			final Realization realization = (Realization) rel;
			for (final NamedElement superEle : realization.getSuppliers()) {
				if (superEle instanceof Classifier) {
					getSubClassifiers(superclassifiers, (Classifier) superEle);
				}
			}
		}
		rels = classifier.getSourceDirectedRelationships(UMLPackage.eINSTANCE.getGeneralization());
		for (final Relationship rel : rels) {
			final Generalization generalization = (Generalization) rel;
			getSubClassifiers(superclassifiers, generalization.getGeneral());
		}
	}

}
