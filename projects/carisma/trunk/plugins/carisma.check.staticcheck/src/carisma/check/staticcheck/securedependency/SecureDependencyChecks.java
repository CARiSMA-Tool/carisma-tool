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
	private AnalysisHost host;
	
	/**
	 * Private constructor.
	 * UMLsec will never be initialized.
	 */
	public SecureDependencyChecks(AnalysisHost host) {
		if (host != null) {
			this.host = host;
		} else {
			this.host = new DummyHost(true);
		}
		
		this.secureDependencyViolations = new ArrayList<SecureDependencyViolation>();
	}
	
	public List<SecureDependencyViolation> getViolations() {
		return Collections.unmodifiableList(secureDependencyViolations);
	}
	
	/**
	 * Checks whether a model fulfills the secure dependency requirement.
	 * @param model
	 * @return
	 */
	public int checkSecureDependency(final Package model, final boolean onlyCheckUsages) {
		for (Usage dep : UMLHelper.getAllElementsOfType(model, Usage.class)) {
			analyzeDependency(dep);
		}
		return secureDependencyViolations.size();
	}
	
	/**
	 * Checks the dependency.
	 * @param dep
	 */
	public void analyzeDependency(Usage dep) {
		List<NamedElement> clients = dep.getClients();
		List<NamedElement> suppliers = dep.getSuppliers();
		for (NamedElement c : clients) {
			for (NamedElement s : suppliers) {
				if (c instanceof Classifier && s instanceof Classifier) {
					secureDependencyViolations.addAll(checkDependency(dep, (Classifier) c, (Classifier) s));
				}
			}
		}
	}
	
	public List<SecureDependencyViolation> checkDependency(Usage dependency, Classifier client, Classifier supplier) {
		host.appendLineToReport("Processing dependency '" + dependency.getQualifiedName()
		        + "' between '" + client.getQualifiedName() + "' and '" + supplier.getQualifiedName() + "'");
		if (!isRelevantDependency(dependency)) {
			host.appendLineToReport("  - not in scope of <<secure dependency>> -> nothing to check!");
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
		List<String> relevantSignaturesForSecrecy = new ArrayList<String>();
		List<String> relevantSignaturesForIntegrity = new ArrayList<String>();
		List<String> relevantSignaturesForHigh = new ArrayList<String>();
		List<String> irrelevantSignatures = new ArrayList<String>();
		for (String sig : operationSignatures) {
			if (allSecrecyTagValuesOfSupplierAndChildren.contains(sig)) {
				relevantSignaturesForSecrecy.add(sig);
			} else if (allIntegrityTagValuesOfSupplierAndChildren.contains(sig)) {
				relevantSignaturesForIntegrity.add(sig);
			} else if (allHighTagValuesOfSupplierAndChildren.contains(sig)) {
				relevantSignaturesForHigh.add(sig);
			} else {
				irrelevantSignatures.add(sig);
			}
		}
		List<String> secrecyTagValuesOfClient = new ArrayList<String>();
		getDistinctTagValues(secrecyTagValuesOfClient, client, UMLsec.TAG_CRITICAL_SECRECY);
		List<String> integrityTagValuesOfClient = new ArrayList<String>();
		getDistinctTagValues(integrityTagValuesOfClient, client, UMLsec.TAG_CRITICAL_INTEGRITY);
		List<String> highTagValuesOfClient = new ArrayList<String>();
		getDistinctTagValues(highTagValuesOfClient, client, UMLsec.TAG_CRITICAL_HIGH);
		
		List<SecureDependencyViolation> errors = new ArrayList<SecureDependencyViolation>();

		if (!secrecyTagValuesOfClient.isEmpty() || !relevantSignaturesForSecrecy.isEmpty()) {
			host.appendLineToReport("  - analyzing secrecy tag values ...");
			String error = checkLists("secrecy", relevantSignaturesForSecrecy, secrecyTagValuesOfClient, irrelevantSignatures);
			if (error != null) {
				errors.add(new SecureDependencyViolation(error, dependency, client, supplier));
				host.appendLineToReport("    " + error);
			}
			if (!UMLsecUtil.hasStereotype(dependency, UMLsec.SECRECY)) {
				errors.add(new SecureDependencyViolation("Dependency misses stereotype <<secrecy>>!", dependency, client, supplier));
				host.appendLineToReport("    Dependency misses stereotype <<secrecy>>!");
			}
		}
		if (!integrityTagValuesOfClient.isEmpty() || !relevantSignaturesForIntegrity.isEmpty()) {
			host.appendLineToReport("  - analyzing integrity tags ...");
			String error = checkLists("integrity", relevantSignaturesForIntegrity, integrityTagValuesOfClient, irrelevantSignatures);
			if (error != null) {
				errors.add(new SecureDependencyViolation(error, dependency, client, supplier));
				host.appendLineToReport("    " + error);
			}
			if (!UMLsecUtil.hasStereotype(dependency, UMLsec.INTEGRITY)) {
				errors.add(new SecureDependencyViolation("Dependency misses stereotype <<integrity>>!", dependency, client, supplier));
				host.appendLineToReport("    Dependency misses stereotype <<integrity>>!");
			}
		}
		if (!highTagValuesOfClient.isEmpty() || !relevantSignaturesForHigh.isEmpty()) {
			host.appendLineToReport("  - analyzing high tags ...");
			String error = checkLists("high", relevantSignaturesForHigh, highTagValuesOfClient, irrelevantSignatures);
			if (error != null) {
				errors.add(new SecureDependencyViolation(error, dependency, client, supplier));
				host.appendLineToReport("    " + error);
			}
			if (!UMLsecUtil.hasStereotype(dependency, UMLsec.HIGH)) {
				errors.add(new SecureDependencyViolation("Dependency misses stereotype <<high>>!", dependency, client, supplier));
				host.appendLineToReport("    Dependency misses stereotype <<high>>!");
			}
		}
		return errors;
	}
	
	private String checkLists(
			final String tagName, 
			final List<String> allSecrecyTagsOfSupplierAndChildren, 
			final List<String> secrecyTagsOfClient, 
			final List<String> irrelevantSignatures) {
		List<String> clientSecrecyTagsNotInSupplier = new ArrayList<String>();
		List<String> secrecyTagsNotInClient = new ArrayList<String>();
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
	/**
	 * Retrieves all distinct tag values of the given tag name from the classifier and all of its subclassifiers.
	 * @param classifier
	 * @param tagName
	 * @return
	 */
	public List<String> getAllDistinctTagValuesOfClassifierAndSubclasses(final Classifier classifier,final String tagName) {
		List<String> distinctTagValues = new ArrayList<String>();
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
	private void getDistinctTagValues(final List<String> distinctTagValues, final Classifier classifier, final String tagName) {
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
	public List<String> getOperationSignatures(final Classifier classifier) {
		List<String> signatures = new ArrayList<String>();
		for (Operation operation : classifier.getAllOperations()) {
			String signature = operation.getName() + "(";
			StringBuffer parameters = new StringBuffer();
			for (Parameter p : operation.getOwnedParameters()) {
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
		List<Classifier> subclassifiers = new ArrayList<Classifier>();
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
		ArrayList<Classifier> superclassifiers = new ArrayList<Classifier>();
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