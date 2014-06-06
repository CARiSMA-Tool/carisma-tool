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
package carisma.modeltype.owl2.hide;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Queue;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.util.EObjectUtil;
import carisma.modeltype.owl2.OWL2ModelHelper;
import carisma.modeltype.owl2.model.owl.Annotation;
import carisma.modeltype.owl2.model.owl.AnnotationByConstant;
import carisma.modeltype.owl2.model.owl.Axiom;
import carisma.modeltype.owl2.model.owl.Declaration;
import carisma.modeltype.owl2.model.owl.Ontology;
import carisma.modeltype.owl2.model.owl.URI;

/**
 * OWL2 HideHelper.
 * @author Marcel Michel
 * 
 */
public class HideHelper {
	
	private HideHelper() {}
	
	private static final String IRI_ABBRIVATED_HIDE = "hide";
	private static final String IRI_ABBRIVATED_UNHIDE = "unhide";
	
	public static List<String> getDerivedHidingInformation(Ontology ontology, boolean includeImports) {
		List<String> result = new ArrayList<String>();
		HashMap<String, Boolean> hm = new HashMap<String, Boolean>();
		Queue<Ontology> ontologyQueue = new LinkedList<Ontology>();
		
		ontologyQueue.offer(ontology);
		while (!ontologyQueue.isEmpty()) {
			Ontology currentOntology = ontologyQueue.poll();
			if (includeImports) {
				for (Ontology o : currentOntology.getImportedOntologies()) {
					ontologyQueue.offer(o);
				}
			}
			Logger.log(LogLevel.DEBUG, 
					"\nCurrent Ontology: "
					+ currentOntology.getOntologyURI().getValue());
			
			for (Annotation a : getAllHidingInformation(currentOntology, false)) {
				String id = ((AnnotationByConstant) a).getAnnotationValue().getLexicalValue().trim();
				if (!hm.containsKey(id)) {
					hm.put(id, isHideInformation(a) ? true : false);
					Logger.log(LogLevel.DEBUG, 
							"Add HidingInformation: "
							+ id 
							+ " - Value: "
							+ (isHideInformation(a) ? true : false));
				} else {
					Logger.log(LogLevel.DEBUG, 
							"Ignored HidingInformation: "
							+ id 
							+ " - Value: "
							+ (isHideInformation(a) ? true : false));
				}
			}
		}
		
		Logger.log(LogLevel.DEBUG, "\n- - - - - S U M M A R Y - - - - - ");
		for (Entry<String, Boolean> e : hm.entrySet()) {
			Logger.log(LogLevel.DEBUG, "Key: " + e.getKey() + " - Value: "
					+ e.getValue().toString());
			if (e.getValue()) {
				result.add(e.getKey());
			}
		}
		
		return result;
	}
	
	public static void applyHidings(Ontology ontology, boolean includeImports) {
		applyHidings(ontology, getDerivedHidingInformation(ontology, includeImports), includeImports);
	}
	
	private static void applyHidings(Ontology ontology, List<String> hidings, boolean includeImports) {
		HashMap<String, Axiom> relevantHidings = OWL2ModelHelper.findAxiomsByIds(ontology, hidings);
		List<Axiom> toBeDeleted = new ArrayList<Axiom>();
		Logger.log(LogLevel.DEBUG, "\n --- Applying Hiding ---");
		for (String id : hidings) {
			if (relevantHidings.containsKey(id)) {
				Axiom currentAxiom = relevantHidings.get(id);
				if (currentAxiom instanceof Declaration) {
					if (checkHide(ontology, (Declaration) currentAxiom, relevantHidings, includeImports)) {
						toBeDeleted.add(currentAxiom);
					} else {
						System.err.println("Error: URI '" 
								+ ((Declaration) currentAxiom).getEntity().getEntityURI().getValue()
								+ "' is still referenced and will be skipped.");
					}
				} else {
					toBeDeleted.add(currentAxiom);
				}
			} else {
				System.err.println("Error: Axiom id '" + id 
						+ "' could not be resolved and will be skipped.");
			}
		}
		
		for (Axiom a : toBeDeleted) {
			Logger.log(LogLevel.DEBUG, "Axiom: " + EObjectUtil.getName(a) + " will be removed");
			EcoreUtil.delete(a);
		}
	}		
	
	private static boolean checkHide(Ontology ontology, Declaration declaration, HashMap<String, Axiom> relevantHidings, boolean includeImports) {
		List<Axiom> occurences = getOccurrences(ontology, 
				declaration.getEntity().getEntityURI().getValue(), 
				includeImports);
		
		for (Axiom axiom : occurences) {
			if (!relevantHidings.containsKey(axiom.getAxiomId())) {
				return false;
			}
		}
		
		return true;
	}
	
	public static List<Axiom> getOccurrences(Ontology ontology, String uri, boolean includeImports) {
		List<Axiom> occurences = new ArrayList<Axiom>();
		List<Ontology> queue = new ArrayList<Ontology>();
		
		queue.add(ontology);
		if (includeImports) {
			queue.addAll(OWL2ModelHelper.getAllImportedOntologies(ontology));
		}
		
		for (Ontology o : queue) {
			for (Axiom a : o.getAxioms()) {
				for (EObject obj : a.eCrossReferences()) {
					EStructuralFeature sf = obj.eClass().getEStructuralFeature("entityURI");
					if (sf != null
							&& ((URI) obj.eGet(sf)).getValue().equalsIgnoreCase(uri)) {
						occurences.add(a);
					}
				}
			}
		}
		return occurences;
	}
	
	public static List<Annotation> getAllHidingInformation(Ontology ontology, boolean includeImports) {
		List<Annotation> hidingInformation = new ArrayList<Annotation>();
		for (Annotation a : ontology.getOntologyAnnotations()) {
			if (isGeneralHidingInformation(a)) {
				hidingInformation.add(a);
			}
		}
		
		if (includeImports) {
			for (Ontology o : ontology.getImportedOntologies()) {
				hidingInformation.addAll(getAllHidingInformation(o, includeImports));
			}
		}
		
		return hidingInformation;
	}
	
	public static boolean isGeneralHidingInformation(Annotation annotation) {
		String entityValue = annotation.getAnnotationProperty().getEntityURI().getValue();
		return entityValue.equalsIgnoreCase(getHideIRI())
				|| entityValue.equalsIgnoreCase(getUnhideIRI());
	}
	
	public static boolean isHideInformation(Annotation annotation) {
		String entityValue = annotation.getAnnotationProperty().getEntityURI().getValue();
		return entityValue.equalsIgnoreCase(getHideIRI());
	}
	
	public static boolean isUnhideInformation(Annotation annotation) {
		String entityValue = annotation.getAnnotationProperty().getEntityURI().getValue();
		return entityValue.equalsIgnoreCase(getUnhideIRI());
	}
	
	public static String getHideIRI() {
		return OWL2ModelHelper.getBaseIRI() + IRI_ABBRIVATED_HIDE;
	}
	
	public static String getUnhideIRI() {
		return OWL2ModelHelper.getBaseIRI() + IRI_ABBRIVATED_UNHIDE;
	}
	
}
