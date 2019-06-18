/*******************************************************************************
 * Copyright (c) 2019 Software Engineering Institute, Universität Koblenz-Landau.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/


package carisma.check;

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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
import org.eclipse.uml2.uml.Parameter;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Realization;
import org.eclipse.uml2.uml.Relationship;
import org.eclipse.uml2.uml.Type;
import org.eclipse.uml2.uml.UMLPackage;


/**
 * Functions to process UMLsec properties.
 * 
 * 
 * @author Volkan Topcu
 *
 */
public final class SecureDependencyInheritanceChecks {

	private List<SecureDependencyInheritanceViolation> secureDependencyViolations;
	private AnalysisHost analysisHost;

	/**
	 * Private constructor. UMLsec will never be initialized.
	 */
	public SecureDependencyInheritanceChecks(AnalysisHost host) {
		if (host != null) {
			this.analysisHost = host;
		} else {
			this.analysisHost = new DummyHost(true);
		}
		this.secureDependencyViolations = new ArrayList<>();
	}

	public List<SecureDependencyInheritanceViolation> getViolations() {
		return Collections.unmodifiableList(this.secureDependencyViolations);
	}

	/**
	 * Checks whether a model fulfills the secure dependency requirement.
	 * 
	 * @param model
	 * @return
	 */
	public int checkSecureDependency(final Package model) {
				
		List<Classifier> classifiersToCheck = new ArrayList<>();
		classifiersToCheck.addAll(UMLHelper.getAllElementsOfType(model, Classifier.class));
		for (Classifier clas : classifiersToCheck) {
			analyzeClassifier(clas, classifiersToCheck);
		}
		
		List<Dependency> dependenciesToCheck = new ArrayList<>();
		dependenciesToCheck.addAll(UMLHelper.getAllElementsOfType(model, Dependency.class));
		for (Dependency dep : dependenciesToCheck) {
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
	 * Checks whether the class has overriding fields that add new security properties (secrecy, integrity). 
	 * Adds secure dependency violation if it does.
	 * 
	 * @param clas The classifier to analyze. 
	 * @param classifiersToCheck All classifiers in the model. 
	 */
	public void analyzeClassifier(Classifier clas, List<Classifier> classifiersToCheck) {
		
		String description;

		List<String> secrecy = new ArrayList<>();
		List<String> integrity = new ArrayList<>(); 
		
		for (Operation operation : clas.getOperations()) {
			for (Operation redefined_operation : operation.getRedefinedOperations()) {
				for (EObject stereotype : clas.getStereotypeApplications()) {
					if (stereotype instanceof critical) {
						critical critical = (critical) stereotype;
						secrecy.addAll(critical.getSecrecy());
						integrity.addAll(critical.getIntegrity());

						for (String sec : secrecy) {
							if (haveSameParameterAndTypes(redefined_operation.getOwnedParameters(), sec, classifiersToCheck)) {
								description =  "Class \"" + clas.getName() + "\" overrides the method \"" + redefined_operation.getName() + "()\" and adds the security property {secrecy}!"; 
								this.secureDependencyViolations.add(new SecureDependencyInheritanceViolation(description, clas, redefined_operation));
								this.analysisHost.appendLineToReport(description);
							}	
						}
						for (String inte : integrity) {
							if (haveSameParameterAndTypes(redefined_operation.getOwnedParameters(), inte, classifiersToCheck)) {
								description =  "Class \"" + clas.getName() + "\" overrides the method \"" + redefined_operation.getName() + "()\" and adds the security property {integrity}!"; 
								this.secureDependencyViolations.add(new SecureDependencyInheritanceViolation(description, clas, redefined_operation));
								this.analysisHost.appendLineToReport(description);
							}
						}
						secrecy.clear();
						integrity.clear();
					}
				}
			}
		}
		
		for (Property attribute : clas.getAttributes()) {
			for (Property red_attribute : attribute.getRedefinedProperties()) {
				for (EObject stereotype : clas.getStereotypeApplications()) {
					if (stereotype instanceof critical) {
						critical critical = (critical) stereotype;
						secrecy.addAll(critical.getSecrecy());
						integrity.addAll(critical.getIntegrity());
					}										
					for (String inte : integrity) {
						if (haveSameType(red_attribute.getType(), inte, classifiersToCheck)) {
							description =  "Class \"" + clas.getName() + "\" overrides the attribute \"" + red_attribute.getName() + "\" and adds the security property {integrity}!"; 
							this.secureDependencyViolations.add(new SecureDependencyInheritanceViolation(description, clas, red_attribute));
							this.analysisHost.appendLineToReport(description);
						}
					}
					
					for (String sec : secrecy) {
						if (haveSameType(red_attribute.getType(), sec, classifiersToCheck)) {
							description =  "Class \"" + clas.getName() + "\" overrides the attribute \"" + red_attribute.getName() + "\" and adds the security property {secrecy}!"; 
							this.secureDependencyViolations.add(new SecureDependencyInheritanceViolation(description, clas, red_attribute));
							this.analysisHost.appendLineToReport(description);
						}
					}
				}
			}
		}	
	}

	/**
	 * Checks if parameters and their types match with the parameters and types of the stereotype signature.
	 * 
	 * @param parameters  List of parameters (with types) of the method. 
	 * @param signature  Signature of the stereotype.
	 * @param classifiersToCheck All classifiers in the model. 
	 * @return true or false 
	 */
	
	public boolean haveSameParameterAndTypes(List<Parameter> parameters, String signature, List<Classifier> classifiersToCheck) {
		
		String type; 
		List<String> names = new ArrayList<>();
		List<String> types = new ArrayList<>();
		List<String> s_names = new ArrayList<>();
		List<String> s_types = new ArrayList<>();
		
		Pattern pattern = Pattern.compile("in\\s(.*?):");
		Pattern pattern2 = Pattern.compile(":\\s(.*?)(,|\\))");
		Matcher matcher = pattern.matcher(signature);
		Matcher matcher2 = pattern2.matcher(signature);
		Pattern pattern3 = Pattern.compile("\\):\\s(.*?)$");
		Matcher matcher3 = pattern3.matcher(signature);
		while (matcher.find()) {
			s_names.add(matcher.group(1));
		}
		while (matcher2.find()) {
			s_types.add(matcher2.group(1));
		}
		if (matcher3.find()) {
			s_types.add(matcher3.group(1));
		}
		int i = 0;
		for (String st : s_types) {
			for (Classifier ctc : classifiersToCheck) {
				if (ctc.getName().equals(st)) {
					for (Classifier p : ctc.allParents()) {
						st = p.getName();
						s_types.set(i, p.getName());
					}
				}
			}
			i++;
		}
		for (Parameter par : parameters) {
			names.add(par.getName());
			type = par.getType().toString();
			types.add(par.getType().toString().substring( (type.indexOf("name:") + 6), type.indexOf(",")));
		}
		if ( (!names.isEmpty()) && s_names.size() != s_types.size()){
			names.remove(names.size()-1);
		}
		return (types.equals(s_types));
	}
	/**
	 * Checks whether the type of the parameter matches with type given by the stereotype signature. 
	 * 
	 * @param type Type of property.
	 * @param signature Signature of the stereotype. 
	 * @param classifiersToCheck All classifiers in the model. 
	 * @return true or false 
	 */
	
	public boolean haveSameType(Type type, String signature, List<Classifier> classifiersToCheck) {
		
		String s_type = "";
		String p_type = "";
		Pattern pattern = Pattern.compile(":\\s(.*?)$");
		Matcher matcher = pattern.matcher(signature);
		if (matcher.find()) {
			s_type = matcher.group(1);
		}
		
		for (Classifier ctc : classifiersToCheck) {
			if (ctc.getName().equals(s_type)) {
				for (Classifier p : ctc.allParents()) {
					s_type = p.getName();
				}
			}
		}
		
		Pattern pattern2 = Pattern.compile("name:\\s(.*?),");
		Matcher matcher2 = pattern2.matcher(type.toString());
		if (matcher2.find()) {
			p_type = matcher2.group(1);
		}
		
		return s_type.equals(p_type); 
	}

	/**
	 * Checks the dependency.
	 * 
	 * @param dep
	 */
	
	public void analyzeDependency(Dependency dep) {
		if (dep instanceof Deployment) {
			return;
		}
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

	public List<SecureDependencyInheritanceViolation> checkDependency(Dependency dependency, Classifier client,
			Classifier supplier) {
		this.analysisHost.appendLineToReport("\nProcessing dependency '" + dependency.getQualifiedName() + "' between '"
				+ client.getQualifiedName() + "' and '" + supplier.getQualifiedName() + "'");
		if (!isRelevantDependency(dependency)) {
			this.analysisHost.appendLineToReport("  - not in scope of <<secure dependency>> -> nothing to check!");
			return Collections.emptyList();
		}

		List<SecureDependencyInheritanceViolation> errors = new ArrayList<>();

		List<String> freshClient = new ArrayList<>();
		List<String> highClient = new ArrayList<>();
		List<String> integrityClient = new ArrayList<>();
		List<String> privacyClient = new ArrayList<>();
		List<String> secrecyClient = new ArrayList<>();

		getCriticalTags(client, freshClient, highClient, integrityClient, privacyClient, secrecyClient);

		ArrayList<String> freshRequiredClient = new ArrayList<String>();
		getRequired(client, freshClient, new ArrayList<String>(), freshRequiredClient);

		ArrayList<String> highRequiredClient = new ArrayList<String>();
		getRequired(client, highClient, new ArrayList<String>(), highRequiredClient);

		ArrayList<String> integrityRequiredClient = new ArrayList<String>();
		getRequired(client, integrityClient, new ArrayList<String>(), integrityRequiredClient);

		ArrayList<String> privacyRequiredClient = new ArrayList<String>();
		getRequired(client, privacyClient, new ArrayList<String>(), privacyRequiredClient);

		ArrayList<String> secrecyRequiredClient = new ArrayList<String>();
		getRequired(client, secrecyClient, new ArrayList<String>(), secrecyRequiredClient);

		List<Classifier> subSuppliers = getSubClassifiers(supplier);
		for (Classifier subSupplier : subSuppliers) {
			if (subSupplier instanceof Interface) {
				continue;
			}
			List<String> signaturesSupplier = getMemberSignatures(subSupplier);

			List<String> freshSupplier = new ArrayList<>();
			List<String> highSupplier = new ArrayList<>();
			List<String> integritySupplier = new ArrayList<>();
			List<String> privacySupplier = new ArrayList<>();
			List<String> secrecySupplier = new ArrayList<>();

			getCriticalTags(subSupplier, freshSupplier, highSupplier, integritySupplier, privacySupplier,
					secrecySupplier);

			//errors.addAll(analyze(subSupplier, client, dependency, freshSupplier, freshRequiredClient, signaturesSupplier, fresh.class));
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

	private void getCriticalTags(Classifier classifier, List<String> fresh, List<String> high, List<String> integrity,
			List<String> privacy, List<String> secrecy) {

		//get stereotypes inside the class 
		for (EObject stereotype : classifier.getStereotypeApplications()) {
			if (stereotype instanceof critical) {
				critical critical = (critical) stereotype;
				fresh.addAll(critical.getFresh());
				high.addAll(critical.getHigh());
				integrity.addAll(critical.getIntegrity());
				privacy.addAll(critical.getPrivacy());
				secrecy.addAll(critical.getSecrecy());
			}
		}
		
		//get stereotypes inside of all parents
		for (Classifier par : classifier.allParents()) {
			for (EObject stereotype : par.getStereotypeApplications()) {
				if (stereotype instanceof critical) {
					critical critical = (critical) stereotype;
					secrecy.addAll(critical.getSecrecy());
					integrity.addAll(critical.getIntegrity());
				}
			}
		}
	}

	private List<String> getRequired(Classifier classifier, Collection<String> criticalTags,
			Collection<String> provided, Collection<String> required) {
		List<String> sigantures = getMemberSignatures(classifier);

		for (String tag : criticalTags) {
			String[] names = tag.split("\\.");
			int length = names.length;
			String signature = names[length - 1].replaceAll(" ", "");
			String v = ":void";
			if (signature.toLowerCase().endsWith(v)) {
				signature = signature.substring(0, signature.length() - v.length());
			}
			if (sigantures.contains(signature)) {
				if (length == 1) {
					provided.add(signature);
				} else {
					if (names[length - 2].equals(classifier.getName())) {
						if (length == 2) {
							provided.add(signature);
						} else {
							boolean equal = true;
							Element owner = classifier.getOwner();
							for (int i = length - 3; i >= 0; i--) {
								String packageName = names[i];
								equal &= owner != null && owner instanceof Package
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
				}
			} else {
				required.add(signature);
			}
		}
		return null;
	}

	public Collection<SecureDependencyInheritanceViolation> analyze(Classifier supplier,Classifier client, Dependency dependency,
			Collection<String> taggedValueSupplier, Collection<String> requiredClient,
			Collection<String> signaturesSupplier, Class<? extends EObject> criticalTag){
		ArrayList<SecureDependencyInheritanceViolation> errors = new ArrayList<SecureDependencyInheritanceViolation>();

		ArrayList<String> providedSupplier = new ArrayList<String>();
		ArrayList<String> requiredSupplier = new ArrayList<String>();
		getRequired(supplier, taggedValueSupplier, providedSupplier, requiredSupplier);
		Collection<String> relevantRequired = intersection(requiredClient, signaturesSupplier);
		if (relevantRequired.size() > 0 || providedSupplier.size() > 0) {
			boolean requiredSubsetOfProvided = providedSupplier.containsAll(relevantRequired);
			if (relevantRequired.size() != providedSupplier.size() || !requiredSubsetOfProvided) {
				if (requiredSubsetOfProvided) {
					List<String> set = new ArrayList<String>(providedSupplier);
					(set).removeAll(relevantRequired);
					String description = '\"'+supplier.getName() + "\" provides {" + criticalTag.getSimpleName() + "}";
					if (set.size() == 1) {
						description += " for the operation \"" + set.get(0) + "\"";
					} else {
						description += " for the operations ";
						if (set.size() == 2) {
							description += "\"" + set.get(0) + "\" and \"" + set.get(1) + "\" ";
						} else {
							int i = 0;
							for (; i < set.size() - 1; i++) {
								description += "\"" + set.get(i) + "\", ";
							}
							description += "\"" + set.get(i) + "\" ";
						}
					}
					description += " for which \"" + client.getName() + "\" does not!";
					errors.add(new SecureDependencyInheritanceViolation(description, dependency, client, supplier));
					this.analysisHost.appendLineToReport("    " + description);
				} else {
					List<String> set = new ArrayList<String>(relevantRequired);
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
							for (; i < set.size() - 1; i++) {
								description += "\"" + set.get(i) + "\", ";
							}
							description += "\"" + set.get(i) + "\" ";
						}
					}
					description += " for which \"" + supplier.getName() + "\" does not does not provide {"
							+ criticalTag.getSimpleName() + "}!";
					errors.add(new SecureDependencyInheritanceViolation(description, dependency, client, supplier));
					this.analysisHost.appendLineToReport("    " + description);
				}
			}
		}
		return errors;
	}

	private Collection<String> intersection(Collection<String> collectionA, Collection<String> collectionB) {
		ArrayList<String> intersection = new ArrayList<String>();
		for (String a : collectionA) {
			for (String b : collectionB) {
				if (a.equals(b)) {
					intersection.add(a);
				}
			}
		}
		return intersection;
	}

	/**
	 * Retrieves all distinct tag values of the given tag name from the
	 * classifier and all of its subclassifiers.
	 * 
	 * @param classifier
	 * @param tagName
	 * @return
	 */
	public static List<String> getAllDistinctTagValuesOfClassifierAndSubclasses(final Classifier classifier,
			final String tagName) {
		List<String> distinctTagValues = new ArrayList<>();
		for (Classifier subClassifier : getSubClassifiers(classifier)) {
			getDistinctTagValues(distinctTagValues, subClassifier, tagName);
		}
		return distinctTagValues;
	}

	/**
	 * Puts all distinct tag values of a given tag name at the <<critical>> app
	 * of a given classifier in a list.
	 * 
	 * @param distinctTagValues
	 *            - list of distinct tag values
	 * @param classifier
	 *            - the given classifier
	 * @param tagName
	 *            - the tag name to collect the values from
	 */
	private static void getDistinctTagValues(final List<String> distinctTagValues, final Classifier classifier,
			final String tagName) {
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
	 * Returns a list of operation signatures of a given classifier. The
	 * signatures look like this: operationName(param1Name:param1Type,
	 * param2Name:param2Type,...):returnType
	 * 
	 * @param classifier
	 *            - the classifier whose operation signatures are to be
	 *            collected
	 * @return - a list of operation signature strings
	 */
	public static List<String> getMemberSignatures(final Classifier classifier) {
		List<String> signatures = new ArrayList<>();
		for (Operation operation : classifier.getAllOperations()) {
			String signature = SignatureHelper.getSignature(operation);
			signatures.add(signature);
		}
		for (Property property : classifier.getAllAttributes()) {
			String signature = SignatureHelper.getSignature(property);
			signatures.add(signature);
		}
		return signatures;
	}

	public static boolean isRelevantDependency(Dependency dependency) {
		boolean isRelevant = !UMLsecUtil.isInScopeOfStereotype(dependency, UMLsec.SECURE_DEPENDENCY);
/*		if (isRelevant) {
			return false;
		} */
		for (EObject stereotype : dependency.getStereotypeApplications()) {
			isRelevant |= stereotype instanceof call;
			isRelevant |= stereotype instanceof send;
		}
		return isRelevant;
	}

	/**
	 * Retrieves a list of subclassifiers of the given classifier, including the
	 * classifier itself.
	 * 
	 * @param classifier
	 *            - the classifier to use as a basis
	 * @return - the list of subclassifiers
	 */
	public static List<Classifier> getSubClassifiers(Classifier classifier) {
		List<Classifier> subclassifiers = new ArrayList<>();
		getSubClassifiers(subclassifiers, classifier);
		return subclassifiers;
	}

	/**
	 * Retrieves all subclassifiers (specializations and interface realizations)
	 * of the given classifier and adds all of them, including the given
	 * classifier to the given list.
	 * 
	 * @param subclassifiers
	 *            - the list of subclassifiers
	 * @param classifier
	 *            - the given classifier
	 */
	private static void getSubClassifiers(List<Classifier> subclassifiers, final Classifier classifier) {
		subclassifiers.add(classifier);
		List<DirectedRelationship> rels = classifier
				.getTargetDirectedRelationships(UMLPackage.eINSTANCE.getRealization());
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
		List<DirectedRelationship> rels = classifier
				.getSourceDirectedRelationships(UMLPackage.eINSTANCE.getRealization());
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
