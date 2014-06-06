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
package carisma.modeltype.owl2.change;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EContentsEList;
import org.eclipse.emf.ecore.util.EContentsEList.FeatureIterator;
import org.eclipse.emf.ecore.util.EcoreUtil;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.util.EObjectUtil;
import carisma.modeltype.owl2.OWL2ModelHelper;
import carisma.modeltype.owl2.model.owl.Annotation;
import carisma.modeltype.owl2.model.owl.AnnotationByConstant;
import carisma.modeltype.owl2.model.owl.Axiom;
import carisma.modeltype.owl2.model.owl.Ontology;
import carisma.modeltype.owl2.model.owl.OwlPackage;
import carisma.modeltype.owl2.util.ClassNameMapper;

public class ChangeHelper {

	private static final String IRI_ABBRIVATED_CHANGE = "evolution";
	private static final String IRI_ABBRIVATED_ADD = "add";
	private static final String IRI_ABBRIVATED_SUBST = "subst";
	private static final String IRI_ABBRIVATED_DEL = "del";
	private static final String IRI_ABBRIVATED_RESET = "reset";
	
	public static void applyChanges(List<ChangeElement> changes) {
		for (ChangeElement change : changes) {
			if (change instanceof AddElement) {
				applyAddElement((AddElement) change);
			} else if (change instanceof DelElement) {
				applyDelElement((DelElement) change);
			} else if (change instanceof SubstElement) {
				applySubstElement((SubstElement) change);
			} else {
				System.err.println("Error: Change Type '"
						+ change.getClass().toString() + "' is unknown.");
			}
		}
	}
	
	public static void applySubstElement(SubstElement substElement) {
		applyDelElement(substElement.getDelElement());
		applyAddElement(substElement.getAddElement());
	}

	public static void applyDelElement(DelElement delElement) {
		EcoreUtil.delete(delElement.getTarget());
	}

	public static void applyAddElement(AddElement addElement) {
		EStructuralFeature sf = addElement.getTarget().eClass()
				.getEStructuralFeature(addElement.getFeatureName());
		if (sf != null) {
			//FIXME: List may be full
			if (sf.getUpperBound() > 1 || sf.getUpperBound() == -1) {
				//FIXME: Should be improved
				EList<EObject> eList = (EList<EObject>) addElement.getTarget().eGet(sf);
				eList.add(addElement.getAdditionalElement());
			} else {
				addElement.getTarget().eSet(sf, addElement.getAdditionalElement());
			}
		} else {
			System.err.println("Error: StructuralFeature " + addElement.getFeatureName() + " not known.");
		}
	}

	public static List<ChangeElement> getAllChangeElements(Ontology ontology, boolean useReset, boolean includeImports) {
		List<ChangeElement> result = new ArrayList<ChangeElement>();
		for (Annotation a : getAllChangeInformation(ontology, useReset, includeImports)) {
			ChangeElement tmpDelta = convertToChangeElement(ontology, a);
			if (tmpDelta != null) {
				result.add(tmpDelta);
			}
		}
		return result;
	}
	
	private static List<Annotation> getAllChangeInformation(Ontology ontology, boolean useReset, boolean includeImports) {
		List<Annotation> changeInformation = new ArrayList<Annotation>();
		ArrayList<String> relevantResets = new ArrayList<String>();
		
		Queue<Ontology> ontologyQueue = new LinkedList<Ontology>();
		ontologyQueue.offer(ontology);
		while (!ontologyQueue.isEmpty()) {
			Ontology currentOntology = ontologyQueue.poll();
			if (includeImports) {
				for (Ontology o : currentOntology.getImportedOntologies()) {
					ontologyQueue.offer(o);
				}
			}
			List<String> usedResets = new ArrayList<String>();
			List<String> newResets = new ArrayList<String>();
			for (Annotation a : currentOntology.getOntologyAnnotations()) {
				if (isChangeInformation(a)) {
					if (isResetInformation(a)) {
						newResets.add(((AnnotationByConstant) a).getAnnotationValue().getLexicalValue().trim());
					} else {
						String changeInformationString =
								((AnnotationByConstant) a).getAnnotationValue().getLexicalValue().trim();
						String AxiomId = changeInformationString.split("\\.")[0];
						if (useReset && relevantResets.contains(AxiomId)) {
							usedResets.add(AxiomId);
						} else {
							changeInformation.add(0, a);
						}
					}
				}
			}
			for (String reset : usedResets) {
				relevantResets.remove(reset);
			}
			for (String reset : newResets) {
				relevantResets.add(reset);
			}
		}
		
		return changeInformation;
	}
	
	private static ChangeElement convertToChangeElement(Ontology ontology, Annotation annotation) {
		String value = ((AnnotationByConstant) annotation).getAnnotationValue().getLexicalValue().trim();

		if (isAddInformation(annotation)) {
			return createAddElement(ontology, value);
		} else if (isDelInformation(annotation)) {
			return createDelElement(ontology, value);
		} else if (isSubstInformation(annotation)) {
			return createSubstElement(ontology, value);
		}
		
		return null;
	}
	
	private static AddElement createAddElement(Ontology ontology, String encodedElement) {
		if (ChangeParser.isAddElement(encodedElement)) {
			HashMap<String, String> parsedAddElement = ChangeParser.parseAddElement(encodedElement); 
			String[] navigation = parsedAddElement.get("navigation").split("\\.");
			
			//FIXME: Performance leak - store it 
			ClassNameMapper mapper = new ClassNameMapper(OwlPackage.eINSTANCE);
			EClass[] eClassNavigation = new EClass[navigation.length - 1];
			for (int i = 0; i < navigation.length - 1; i++) {
				EClass eClass = mapper.getEClassByName(navigation[i]);
				if (eClass != null) {
					eClassNavigation[i] = eClass;
				} else {
					System.err.println("Error: Could not map '" + navigation[i] + "'");
					return null;
				}
			}
				
			String axiomId = parsedAddElement.get("axiomid");
			String feature = navigation[navigation.length - 1];
			EObject source = OWL2ModelHelper.findOntologyOrAxiomById(ontology, axiomId);
				
			if (source != null) {
				EObject target = traceToEClassBFS(eClassNavigation, source, feature, null);
				
				if (target != null) {
					String definedTypeString = parsedAddElement.get("object");
					String definedValueString = parsedAddElement.get("objectproperties");
					
					Logger.log(LogLevel.DEBUG, "\n- - - Creating AddElement - - -");
					Logger.log(LogLevel.DEBUG, "AddElement: Type '" + definedTypeString + "'");
					Logger.log(LogLevel.DEBUG, "AddElement: Value '" + definedValueString + "'");
					Logger.log(LogLevel.DEBUG, "AddElement: Selected Element '" + target.eClass().getName() + "'");
					Logger.log(LogLevel.DEBUG, "AddElement: Feature '" + feature + "'");
					
					ElementType type = null;
					for (ElementType e : ElementType.values()) {
						if (e.getValue().equalsIgnoreCase(definedTypeString)) {
							type = e;
							break;
						}
						
					}
					if (type == null) {
						System.err.println("Error: Type '" + definedTypeString + "' unknwon.");
						return null;
					} else {
						EObject definedObject = ElementFactory.createElement(type, definedValueString);
						return new AddElement(target, feature, type, definedObject);
					}
				} else {
					System.err.println("Error: Failed to resolve target element with feature '" + feature + "'.");
					return null;
				}
				
			} else {
				System.err.println("Error: Failed to resolve id '" + axiomId + "'.");
				return null;
			}
				
		}
		System.err.println("Error: Failed to create Add-Element '" + encodedElement + "'.");
		return null;
	}
	
	private static DelElement createDelElement(Ontology ontology, String encodedElement) {
		if (ChangeParser.isDelElement(encodedElement)) {
			HashMap<String, String> parsedDelElement = ChangeParser.parseDelElement(encodedElement);
			String axiomId = parsedDelElement.get("axiomid");
			String[] navigation = parsedDelElement.get("navigation").split("\\.");
			
			//FIXME: Performance leak - store it 
			ClassNameMapper mapper = new ClassNameMapper(OwlPackage.eINSTANCE);
			EClass[] eClassNavigation = new EClass[navigation.length - 1];
			int selectedIndex = -1;
			for (int i = 0; i < navigation.length - 1; i++) {
				if (navigation[i].substring(0, 1).equals("!")) {
					selectedIndex = i;
					navigation[i] = navigation[i].substring(1, navigation[i].length());
				}
				EClass eClass = mapper.getEClassByName(navigation[i]);
				if (eClass != null) {
					eClassNavigation[i] = eClass;
				} else {
					System.err.println("Error: Could not map '" + navigation[i] + "'");
					return null;
				}
			}
			
			Axiom source = OWL2ModelHelper.findAxiomById(ontology, axiomId);
			if (source != null) {
				
				String property = navigation[navigation.length - 1];
				String value = parsedDelElement.get("objectvalue");
				
				Logger.log(LogLevel.DEBUG, "\n- - - Creating DelElement - - -");
				Logger.log(LogLevel.DEBUG, "DelElement: Property '" + property + "'");
				Logger.log(LogLevel.DEBUG, "DelElement: Value '" + value + "'");
				Logger.log(LogLevel.DEBUG, "DelElement: Selected Element '" + source.eClass().getName() + "'");
				
				EObject target = traceToEClass(eClassNavigation, selectedIndex, source, property, value);
				if (target != null) {
					return new DelElement(target);
				} else {
					System.err.println("Error: Failed to locate object with specification '" + encodedElement + "'.");
				}
			} else {
				System.err.println("Error: Failed to resolve id '" + axiomId + "'.");
				return null;
			}
		}
		System.err.println("Error: Failed to create Deletion-Element '" + encodedElement + "'.");
		return null;
	}
	
	private static SubstElement createSubstElement(Ontology ontology, String encodedElement) {
		if (ChangeParser.isSubstElement(encodedElement)) {
			HashMap<String, String> parsedSubstElement = ChangeParser.parseSubstElement(encodedElement);
			
			String[] navigation = parsedSubstElement.get("navigation").split("\\.");
			//FIXME: Performance leak - store it 
			ClassNameMapper mapper = new ClassNameMapper(OwlPackage.eINSTANCE);
			EClass[] eClassNavigation = new EClass[navigation.length - 1];
			int selectedIndex = -1;
			for (int i = 0; i < navigation.length - 1; i++) {
				if (navigation[i].substring(0, 1).equals("!")) {
					selectedIndex = i;
					navigation[i] = navigation[i].substring(1, navigation[i].length());
				}
				EClass eClass = mapper.getEClassByName(navigation[i]);
				if (eClass != null) {
					eClassNavigation[i] = eClass;
				} else {
					System.err.println("Error: Could not map '" + navigation[i] + "'");
					return null;
				}
			}
			String axiomId = parsedSubstElement.get("axiomid");
			String property = navigation[navigation.length - 1];
			String value = parsedSubstElement.get("objectvalue");
			Axiom source = OWL2ModelHelper.findAxiomById(ontology, axiomId);
			
			List<EObject> trace = traceToEClass(eClassNavigation, source, property, value);
			if (trace.size() == 0) {
				System.err.println("Error: Could not create trace of SubstElement '" 
						+ encodedElement + "'");
				return null;
			}
			EObject container = trace.get(selectedIndex);
			EContentsEList.FeatureIterator<EObject> featureIterator = (FeatureIterator<EObject>) container.eCrossReferences().iterator();
			String featureName = null;
			while (featureIterator.hasNext()) {
				EObject referencedEObject = featureIterator.next();
				if (referencedEObject.equals(trace.get(selectedIndex + 1))) {
					EStructuralFeature feature = featureIterator.feature();
					featureName = feature.getName();
					break;
				}
				
			}
			if (featureName == null) {
				System.err.println("Error: Could not resolve feature of '" 
						+ encodedElement + "'");
				return null;
			}
			
			String encodedAddElement = axiomId;
			for (int i = 0; i < selectedIndex; i++) {
				encodedAddElement += "." + navigation[i];
			}
			encodedAddElement += "." + featureName;
			encodedAddElement += "->" + parsedSubstElement.get("object") 
										+ "(" + parsedSubstElement.get("objectproperties") + ")";
			AddElement addElement = createAddElement(ontology, encodedAddElement);
			
			String encodedDelElement = parsedSubstElement.get("axiomid")
										+ "." + parsedSubstElement.get("navigation")
										+ "=\"" + parsedSubstElement.get("objectvalue") + "\""; 
			DelElement delElement = createDelElement(ontology, encodedDelElement);
			
			if (addElement != null && delElement != null) {
				SubstElement substElement = new SubstElement(addElement, delElement);
				return substElement;
			}
		}
		System.err.println("Error: Failed to create Substitution-Element '" + encodedElement + "'.");
		return null;
	}
	
	private static EObject traceToEClass(EClass[] trace, int selectedElement, EObject startElement, String property, String value) {
		fullTrace = new ArrayList<EObject>();
		skipRecursivePaths = false;
		Logger.log(LogLevel.DEBUG, "\n- - - Begin Trace - - -");
		traceToEClass(trace, new ArrayList<EObject>(Arrays.asList(startElement)), property, value);
		return (fullTrace.size() > 0) ? fullTrace.get(selectedElement + 1) : null;
	}
	
	private static List<EObject> traceToEClass(EClass[] trace, EObject startElement, String property, String value) {
		fullTrace = new ArrayList<EObject>();
		skipRecursivePaths = false;
		Logger.log(LogLevel.DEBUG, "\n- - - Begin Trace - - -");
		traceToEClass(trace, new ArrayList<EObject>(Arrays.asList(startElement)), property, value);
		return fullTrace;
	}
	
	
	private static List<EObject> fullTrace = null;
	private static boolean skipRecursivePaths = false;
	// Backtracking
	private static void traceToEClass(EClass[] trace, List<EObject> currentTrace, String property, String value) {
		if (currentTrace.size() - 1 < trace.length) {
			EObject currentElement = currentTrace.get(currentTrace.size() - 1);
			for (int i = 0; i < currentElement.eCrossReferences().size() && !skipRecursivePaths; i++) {
				EObject currentCrossReference = currentElement.eCrossReferences().get(i);
				if (trace[currentTrace.size() - 1].isInstance(currentCrossReference)) {
					Logger.log(LogLevel.DEBUG, "Trace: Following '" + EObjectUtil.getName(currentCrossReference) + "'.");
					currentTrace.add(currentCrossReference);
					traceToEClass(trace, currentTrace, property, value);
					currentTrace.remove(currentTrace.size() - 1);
				} else {
					Logger.log(LogLevel.DEBUG, "Trace: Skipping '" + EObjectUtil.getName(currentCrossReference) + "' does not match '" 
							+ EObjectUtil.getName(trace[currentTrace.size() - 1]) + "'");
				}
			}
		}
		if (trace.length == currentTrace.size() - 1) {
			EObject currentElement = currentTrace.get(currentTrace.size() - 1);
			EStructuralFeature sf = currentElement.eClass().getEStructuralFeature(property);
			if (sf != null && currentElement.eGet(sf).toString().equals(value)) {
				Logger.log(LogLevel.DEBUG, "Trace: Found defined object: '"
						+ EObjectUtil.getName(currentElement) + "'");
				fullTrace = new ArrayList<EObject>(currentTrace);
				skipRecursivePaths = true;
				return;
			} else {
				Logger.log(LogLevel.DEBUG, "Trace: Object '"
						+ EObjectUtil.getName(currentElement) + "' does not contain desired feature.");
			}
		} 
	}
	
	// BFS
	private static EObject traceToEClassBFS(EClass[] trace, EObject startElement, String property, String value) {
		Queue<EObject> currentLevel = new LinkedList<EObject>();
		Queue<EObject> nextLevel = new LinkedList<EObject>();
		
		EObject currentObject = null;
		currentLevel.offer(startElement);
		int depth = 0;
		
		Logger.log(LogLevel.DEBUG, "\n- - - Begin Trace - - -");
		while (!currentLevel.isEmpty()) {
			currentObject = currentLevel.poll();
			
			if (trace.length == depth) {
				EStructuralFeature sf = currentObject.eClass().getEStructuralFeature(property);
				
				if (value == null) {
					Logger.log(LogLevel.DEBUG, "Trace: Found defined object (value ignored): '"
							+ EObjectUtil.getName(currentObject) + "'");
					return currentObject;
				} else if (currentObject.eGet(sf).toString().equals(value)) {
					Logger.log(LogLevel.DEBUG, "Trace: Found defined object: '"
							+ EObjectUtil.getName(currentObject) + "'");
					return currentObject;
				} else {
					Logger.log(LogLevel.DEBUG, "Trace: Object '"
							+ EObjectUtil.getName(currentObject) + "' does not contain desired feature.");
				}
			}
			
			for (EObject ref : currentObject.eCrossReferences()) {
				if (trace[depth].isInstance(ref)) {
					nextLevel.offer(ref);
					Logger.log(LogLevel.DEBUG, "Trace: Added '" + EObjectUtil.getName(ref) + "' to Trace.");
				} else {
					Logger.log(LogLevel.DEBUG, "Trace: Skipping '" + EObjectUtil.getName(ref) + "' does not match '" 
							+ EObjectUtil.getName(trace[depth]) + "'");
				}
			}
			
			if (currentLevel.isEmpty() && depth < trace.length) {
				depth++;
				Logger.log(LogLevel.DEBUG, "Trace: Going to level " + depth);
				while (!nextLevel.isEmpty()) {
					currentLevel.offer(nextLevel.poll());
				}
			}
		}
		return null;
	}
	
	public static boolean isAddInformation(Annotation annotation) {
		return annotation.getAnnotationProperty()
				.getEntityURI().getValue().equalsIgnoreCase(getChangeIRI() + "/" + IRI_ABBRIVATED_ADD);
	}
	
	public static boolean isDelInformation(Annotation annotation) {
		return false;
				/*annotation.getAnnotationProperty()
				.getEntityURI().getValue().equalsIgnoreCase(getChangeIRI() + "/" + IRI_ABBRIVATED_DEL);*/
	}
	
	public static boolean isSubstInformation(Annotation annotation) {
		return annotation.getAnnotationProperty()
				.getEntityURI().getValue().equalsIgnoreCase(getChangeIRI() + "/" + IRI_ABBRIVATED_SUBST);
	}
	
	public static boolean isResetInformation(Annotation annotation) {
		return annotation.getAnnotationProperty()
				.getEntityURI().getValue().equalsIgnoreCase(getChangeIRI() + "/" + IRI_ABBRIVATED_RESET);
	}
	
	public static boolean isChangeInformation(Annotation annotation) {
		return annotation.getAnnotationProperty()
				.getEntityURI().getValue().contains(getChangeIRI());
	}
	
	public static String getChangeIRI() {
		return OWL2ModelHelper.getBaseIRI() + IRI_ABBRIVATED_CHANGE;
	}
}
