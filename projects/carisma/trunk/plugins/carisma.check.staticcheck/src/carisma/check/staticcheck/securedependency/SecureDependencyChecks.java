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
import java.util.Collections;
import java.util.List;

import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.DirectedRelationship;
import org.eclipse.uml2.uml.Generalization;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Parameter;
import org.eclipse.uml2.uml.Realization;
import org.eclipse.uml2.uml.Relationship;
import org.eclipse.uml2.uml.UMLPackage;
import org.eclipse.uml2.uml.Usage;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * Functions to process UMLsec properties.
 * @author Sven Wenzel
 *
 */
public final class SecureDependencyChecks {
	
	private List<SecureDependencyViolation> secureDependencyViolations;
	private AnalysisHost analysisHost;
	
	/**
	 * Private constructor.
	 * UMLsec will never be initialized.
	 */
	public SecureDependencyChecks(AnalysisHost host) {
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
	 * @param model
	 * @return
	 */
	public int checkSecureDependency(final Package model, final boolean onlyCheckUsages) {
		List<Dependency> dependenciesToCheck = new ArrayList<>();
		if (onlyCheckUsages) {
			dependenciesToCheck.addAll(UMLHelper.getAllElementsOfType(model, Usage.class));
		} else {
			dependenciesToCheck.addAll(UMLHelper.getAllElementsOfType(model, Dependency.class));
		}
		for (Dependency dep : dependenciesToCheck) {
			analyzeDependency(dep);
		}
		return this.secureDependencyViolations.size();
	}
	
	/**
	 * Checks the dependency.
	 * @param dep
	 */
	public void analyzeDependency(Dependency dep) {
		List<NamedElement> clients = dep.getClients();
		List<NamedElement> suppliers = dep.getSuppliers();
		for (NamedElement c : clients) {
			for (NamedElement s : suppliers) {
				if (c instanceof Classifier && s instanceof Classifier) {
					this.secureDependencyViolations.addAll(checkDependency(dep, (Classifier) c, (Classifier) s));
				}
			}
		}
	}
	
	public List<SecureDependencyViolation> checkDependency(Dependency dependency, Classifier client, Classifier supplier) {
		this.analysisHost.appendLineToReport("Processing dependency '" + dependency.getQualifiedName()
		        + "' between '" + client.getQualifiedName() + "' and '" + supplier.getQualifiedName() + "'");
		if (!isRelevantDependency(dependency)) {
			this.analysisHost.appendLineToReport("  - not in scope of <<secure dependency>> -> nothing to check!");
			return Collections.emptyList();
		}
		// get messages of supplier
		// get critical lists of supplier and all subclasses/implementations
		// for each message 
		// - if in critical lists it has to be in clients critical list
		// - if not in critical lists is must not be in clients critical list
		//FIXME: "in C if and only if it appears in D" -> both directions must be checked
		List<String> operationSignatures = getOperationSignatures(supplier);
		List<String> allSecrecyTagValuesOfSupplierAndChildren = 
				getAllDistinctTagValuesOfClassifierAndSubclasses(supplier, UMLsec.TAG_CRITICAL_SECRECY);
		List<String> allIntegrityTagValuesOfSupplierAndChildren = 
				getAllDistinctTagValuesOfClassifierAndSubclasses(supplier, UMLsec.TAG_CRITICAL_INTEGRITY);
		List<String> allHighTagValuesOfSupplierAndChildren = 
				getAllDistinctTagValuesOfClassifierAndSubclasses(supplier, UMLsec.TAG_CRITICAL_HIGH);
		List<String> allPrivacyTagValuesOfSupplierAndChildren = 
				getAllDistinctTagValuesOfClassifierAndSubclasses(supplier, UMLsec.TAG_CRITICAL_PRIVACY);
		List<String> relevantSignaturesForSecrecy = new ArrayList<>();
		List<String> relevantSignaturesForPrivacy = new ArrayList<>();
		List<String> relevantSignaturesForIntegrity = new ArrayList<>();
		List<String> relevantSignaturesForHigh = new ArrayList<>();
		List<String> irrelevantSignatures = new ArrayList<>();
		for (String sig : operationSignatures) {
			if (allSecrecyTagValuesOfSupplierAndChildren.contains(sig)) {
				relevantSignaturesForSecrecy.add(sig);
			} else if (allIntegrityTagValuesOfSupplierAndChildren.contains(sig)) {
				relevantSignaturesForIntegrity.add(sig);
			} else if (allHighTagValuesOfSupplierAndChildren.contains(sig)) {
				relevantSignaturesForHigh.add(sig);
			} else if (allPrivacyTagValuesOfSupplierAndChildren.contains(sig)) {
				relevantSignaturesForPrivacy.add(sig);
			} else {
				
				irrelevantSignatures.add(sig);
			}
		}
		List<String> secrecyTagValuesOfClient = new ArrayList<>();
		getDistinctTagValues(secrecyTagValuesOfClient, client, UMLsec.TAG_CRITICAL_SECRECY);
		List<String> privacyTagValuesOfClient = new ArrayList<>();
		getDistinctTagValues(privacyTagValuesOfClient, client, UMLsec.TAG_CRITICAL_PRIVACY);
		List<String> integrityTagValuesOfClient = new ArrayList<>();
		getDistinctTagValues(integrityTagValuesOfClient, client, UMLsec.TAG_CRITICAL_INTEGRITY);
		List<String> highTagValuesOfClient = new ArrayList<>();
		getDistinctTagValues(highTagValuesOfClient, client, UMLsec.TAG_CRITICAL_HIGH);
		
		List<SecureDependencyViolation> errors = new ArrayList<>();

		if (!secrecyTagValuesOfClient.isEmpty() || !relevantSignaturesForSecrecy.isEmpty()) {
			this.analysisHost.appendLineToReport("  - analyzing secrecy tag values ...");
			String error = checkLists("secrecy", relevantSignaturesForSecrecy, secrecyTagValuesOfClient, irrelevantSignatures);
			if (error != null) {
				errors.add(new SecureDependencyViolation(error, dependency, client, supplier));
				this.analysisHost.appendLineToReport("    " + error);
			}
			if (!UMLsecUtil.hasStereotype(dependency, UMLsec.SECRECY)) {
				errors.add(new SecureDependencyViolation("Dependency misses stereotype <<secrecy>>!", dependency, client, supplier));
				this.analysisHost.appendLineToReport("    Dependency misses stereotype <<secrecy>>!");
			}
		}
		if (!privacyTagValuesOfClient.isEmpty() || !relevantSignaturesForPrivacy.isEmpty()) {
			this.analysisHost.appendLineToReport("  - analyzing privacy tag values ...");
			String error = privacyCheckLists("privacy", relevantSignaturesForPrivacy, privacyTagValuesOfClient, irrelevantSignatures);
			if (error != null) {
				errors.add(new SecureDependencyViolation(error, dependency, client, supplier));
				this.analysisHost.appendLineToReport("    " + error);
			}
			if (!UMLsecUtil.hasStereotype(dependency, UMLsec.PRIVACY)) {
				errors.add(new SecureDependencyViolation("Dependency misses stereotype <<privacy>>!", dependency, client, supplier));
				this.analysisHost.appendLineToReport("    Dependency misses stereotype <<privacy>>!");
			}
		}
		if (!integrityTagValuesOfClient.isEmpty() || !relevantSignaturesForIntegrity.isEmpty()) {
			this.analysisHost.appendLineToReport("  - analyzing integrity tags ...");
			String error = checkLists("integrity", relevantSignaturesForIntegrity, integrityTagValuesOfClient, irrelevantSignatures);
			if (error != null) {
				errors.add(new SecureDependencyViolation(error, dependency, client, supplier));
				this.analysisHost.appendLineToReport("    " + error);
			}
			if (!UMLsecUtil.hasStereotype(dependency, UMLsec.INTEGRITY)) {
				errors.add(new SecureDependencyViolation("Dependency misses stereotype <<integrity>>!", dependency, client, supplier));
				this.analysisHost.appendLineToReport("    Dependency misses stereotype <<integrity>>!");
			}
		}
		if (!highTagValuesOfClient.isEmpty() || !relevantSignaturesForHigh.isEmpty()) {
			this.analysisHost.appendLineToReport("  - analyzing high tags ...");
			String error = checkLists("high", relevantSignaturesForHigh, highTagValuesOfClient, irrelevantSignatures);
			if (error != null) {
				errors.add(new SecureDependencyViolation(error, dependency, client, supplier));
				this.analysisHost.appendLineToReport("    " + error);
			}
			if (!UMLsecUtil.hasStereotype(dependency, UMLsec.HIGH)) {
				errors.add(new SecureDependencyViolation("Dependency misses stereotype <<high>>!", dependency, client, supplier));
				this.analysisHost.appendLineToReport("    Dependency misses stereotype <<high>>!");
			}
		}
		return errors;
	}
	
	private static String checkLists(
			final String tagName, 
			final List<String> allSecrecyTagsOfSupplierAndChildren, 
			final List<String> secrecyTagsOfClient, 
			final List<String> irrelevantSignatures) {
		List<String> clientSecrecyTagsNotInSupplier = new ArrayList<>();
		List<String> secrecyTagsNotInClient = new ArrayList<>();
		for (String s : allSecrecyTagsOfSupplierAndChildren) {
			if (!secrecyTagsOfClient.contains(s)) {
				secrecyTagsNotInClient.add(s);
			}
		}
		for (String s : secrecyTagsOfClient) {
			if (!allSecrecyTagsOfSupplierAndChildren.contains(s) && irrelevantSignatures.contains(s)) {
				clientSecrecyTagsNotInSupplier.add(s);
			}
		}
		String error = null;
		if (!secrecyTagsNotInClient.isEmpty()) {
			error = "Supplier or one of its subelements defines {" + tagName + "=" + toString(secrecyTagsNotInClient) + "}, but client does not!";
		}
		String tocnis = null;
		if (!clientSecrecyTagsNotInSupplier.isEmpty()) {
			tocnis = "Client defines {" + tagName + "=" + toString(clientSecrecyTagsNotInSupplier) + "}, but neither supplier nor subelement of supplier does!";
		}
		if (error == null && tocnis != null) {
			error = tocnis;
		} else if (error != null && tocnis != null) {
			error = error + " And c" + tocnis.substring(1);
		}
		return error;
	}
	
	
	
	
	private static String privacyCheckLists(
			final String tagName, 
			final List<String> allPrivacyTagsOfSupplierAndChildren, 
			final List<String> PrivacyTagsOfClient, 
			final List<String> irrelevantSignatures) {
		List<String> clientPrivacyTagsNotInSupplier = new ArrayList<>();
		List<String> privacyTagsNotInClient = new ArrayList<>();
		for (String s : allPrivacyTagsOfSupplierAndChildren) {
			if (!PrivacyTagsOfClient.contains(s)) {
				privacyTagsNotInClient.add(s);
			}
		}
		for (String s : PrivacyTagsOfClient) {
			if (!allPrivacyTagsOfSupplierAndChildren.contains(s) && irrelevantSignatures.contains(s)) {
				clientPrivacyTagsNotInSupplier.add(s);
			}
		}
		String error = null;
		if (!privacyTagsNotInClient.isEmpty()) {
			error = "Supplier or one of its subelements defines {" + tagName + "=" + toString(privacyTagsNotInClient) + "}, but client does not!";
		}
		String tocnis = null;
		if (!clientPrivacyTagsNotInSupplier.isEmpty()) {
			tocnis = "Client defines {" + tagName + "=" + toString(clientPrivacyTagsNotInSupplier) + "}, but neither supplier nor subelement of supplier does!";
		}
		if (error == null && tocnis != null) {
			error = tocnis;
		} else if (error != null && tocnis != null) {
			error = error + " And c" + tocnis.substring(1);
		}
		return error;
	}
	
	/**
	 * Retrieves all distinct tag values of the given tag name from the classifier and all of its subclassifiers.
	 * @param classifier
	 * @param tagName
	 * @return
	 */
	public static List<String> getAllDistinctTagValuesOfClassifierAndSubclasses(final Classifier classifier,final String tagName) {
		List<String> distinctTagValues = new ArrayList<>();
		for (Classifier subClassifier : getSubClassifiers(classifier)) {
			getDistinctTagValues(distinctTagValues, subClassifier, tagName);
		}
		return distinctTagValues;
	}
	
	/**
	 * Puts all distinct tag values of a given tag name at the <<critical>> app of a given classifier in a list.
	 * @param distinctTagValues - list of distinct tag values
	 * @param classifier - the given classifier
	 * @param tagName - the tag name to collect the values from
	 */
	private static void getDistinctTagValues(final List<String> distinctTagValues, final Classifier classifier, final String tagName) {
		StereotypeApplication criticalApp = UMLsecUtil.getStereotypeApplication(classifier, UMLsec.CRITICAL);
		if (criticalApp != null) {
			List<String> tagValues = UMLsecUtil.getStringValues(tagName, UMLsec.CRITICAL, classifier);
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
	 * Returns a list of operation signatures of a given classifier. The signatures look like this:
	 * operationName(param1Name:param1Type, param2Name:param2Type,...):returnType 
	 * @param classifier - the classifier whose operation signatures are to be collected
	 * @return - a list of operation signature strings
	 */
	public static List<String> getOperationSignatures(final Classifier classifier) {
		List<String> signatures = new ArrayList<>();
		for (Operation operation : classifier.getAllOperations()) {
			String signature = operation.getName() + "(";
			StringBuffer parameters = new StringBuffer();
			for (Parameter p : operation.getOwnedParameters()) {
				if (p.equals(operation.getReturnResult())) {
					continue;
				}
				String type = "void";
				if (p.getType() != null) {
					type = p.getType().getName();
				}
				parameters.append(p.getName());
				parameters.append(":");
				parameters.append(type);
				parameters.append(", ");
			}
			if (!"".equals(parameters.toString())) {
				signature += parameters.substring(0, parameters.lastIndexOf(", "));
			}
//			signature += ")";
			
			if (operation.getType() != null) {
				signature += "):" + operation.getType().getName();
			} else {
				signature += ")";
			}
			signatures.add(signature);
		}
		return signatures;
	}
	
	/**
	 * Converts a list of strings to a single string.
	 * @param list
	 * @return
	 */
	private static String toString(List<String> list) {
		StringBuffer result = new StringBuffer(", ");
		for (String s : list) {
			result.append(s);
		}
		return result.substring(2);
	}
	
	public static boolean isRelevantDependency(Dependency dependency) {
		if ((UMLHelper.isStereotypeApplied(dependency, "call") || UMLHelper.isStereotypeApplied(dependency, "send")) 
				&& (UMLsecUtil.isInScopeOfStereotype(dependency, UMLsec.SECURE_DEPENDENCY))) {
			return true;
		}
		return false;
	}
	
	/**
	 * Retrieves a list of subclassifiers of the given classifier, including the classifier itself.
	 * @param classifier - the classifier to use as a basis
	 * @return - the list of subclassifiers
	 */
	public static List<Classifier> getSubClassifiers(Classifier classifier) {
		List<Classifier> subclassifiers = new ArrayList<>();
		getSubClassifiers(subclassifiers, classifier);
		return subclassifiers;
	}
	
	/**
	 * Retrieves all subclassifiers (specializations and interface realizations) of the given classifier
	 * and adds all of them, including the given classifier to the given list.
	 * @param subclassifiers - the list of subclassifiers
	 * @param classifier - the given classifier
	 */
	private static void getSubClassifiers(List<Classifier> subclassifiers, final Classifier classifier) {
		subclassifiers.add(classifier);		
		List<DirectedRelationship> rels = classifier.getTargetDirectedRelationships(UMLPackage.eINSTANCE.getRealization());
		for (Relationship rel : rels) {
			Realization realization = (Realization) rel;
			for (NamedElement sub : realization.getClients()) {
				if (sub instanceof Classifier) {
					getSubClassifiers(subclassifiers, (Classifier) sub);
				}
			}
		}		
		rels = classifier.getTargetDirectedRelationships(UMLPackage.eINSTANCE.getGeneralization());
		for (Relationship rel : rels) {
			Generalization generalization = (Generalization) rel;
			getSubClassifiers(subclassifiers, generalization.getSpecific());
		}		
	}
	
	public static List<Classifier> getSuperClassifiers(Classifier classifier) {
		ArrayList<Classifier> superclassifiers = new ArrayList<>();
		getSuperClassifiers(superclassifiers, classifier);
		return superclassifiers;
	}
	
	private static void getSuperClassifiers(List<Classifier> superclassifiers, Classifier classifier) {
		superclassifiers.add(classifier);		
		List<DirectedRelationship> rels = classifier.getSourceDirectedRelationships(UMLPackage.eINSTANCE.getRealization());
		for (Relationship rel : rels) {
			Realization realization = (Realization) rel;
			for (NamedElement superEle : realization.getSuppliers()) {
				if (superEle instanceof Classifier) {
					getSubClassifiers(superclassifiers, (Classifier) superEle);
				}
			}
		}		
		rels = classifier.getSourceDirectedRelationships(UMLPackage.eINSTANCE.getGeneralization());
		for (Relationship rel : rels) {
			Generalization generalization = (Generalization) rel;
			getSubClassifiers(superclassifiers, generalization.getGeneral());
		}		
	}
	
}
