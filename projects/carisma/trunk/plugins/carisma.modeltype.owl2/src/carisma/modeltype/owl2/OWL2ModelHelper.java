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
package carisma.modeltype.owl2;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;

import carisma.modeltype.owl2.model.owl.Axiom;
import carisma.modeltype.owl2.model.owl.Ontology;

/**
 * OWL2 ModelHelper.
 * @author Marcel Michel
 * 
 */
public class OWL2ModelHelper {

	private OWL2ModelHelper() {}
	
	private static final String IRI_BASE = "http://carimsa/";
	
	private static final String IRI_ABBRIVATED_ID = "id";
	
	/**
     * Returns a list of all elements of the given type.
     * 
     * @param <T> The type of the desired element
     * @param root The root element of the model
     * @param type The instance of the desired element
     * @return returns a list of all elements of the given type
     */
	@SuppressWarnings("unchecked")
	public static <T> List<T> getAllElementsOfType(final EObject root, final Class<T> type) {
		ArrayList<T> result = new ArrayList<T>();
		TreeIterator<EObject> iterator = root.eAllContents();
		while (iterator.hasNext()) {
			EObject element = iterator.next();
			if (type.isInstance(element)) {
				result.add((T) element);
			}
		}
		return result;
	}
	
	public static EObject findOntologyOrAxiomById(final EObject root, final String id) {
		TreeIterator<EObject> iterator = root.eAllContents();
		while (iterator.hasNext()) {
			EObject element = iterator.next();
			if (element instanceof Axiom
				&& ((Axiom) element).getAxiomId().equals(id)) {
				return element;
			} else if (element instanceof Ontology
					&& ((Ontology) element).getOntologyId().equals(id)) {
					return element;
				}
		}
		return null;
	}
	
	public static Axiom findAxiomById(final EObject root, final String id) {
		TreeIterator<EObject> iterator = root.eAllContents();
		while (iterator.hasNext()) {
			EObject element = iterator.next();
			if (element instanceof Axiom
				&& ((Axiom) element).getAxiomId().equals(id)) {
				return (Axiom) element;
			}
		}
		return null;
	}
	
	public static HashMap<String, Axiom> findAxiomsByIds(final EObject root, final List<String> ids) {
		HashMap<String, Axiom> result = new HashMap<String, Axiom>();
		TreeIterator<EObject> iterator = root.eAllContents();
		while (iterator.hasNext()) {
			EObject element = iterator.next();
			if (element instanceof Axiom
				&& ids.contains(((Axiom) element).getAxiomId())) {
				result.put(((Axiom) element).getAxiomId(), (Axiom) element);
			}
		}
		return result;
	}
	
	public static List<Ontology> getAllImportedOntologies(Ontology ontology) {
		return getAllImportedOntologies(ontology, new ArrayList<Ontology>());
	}
	
	private static List<Ontology> getAllImportedOntologies(Ontology ontology, List<Ontology> summedImports) {
		List<Ontology> imports = ontology.getImportedOntologies();
		
		summedImports.addAll(imports);
		for (Ontology importedOntology : imports) {
			getAllImportedOntologies(importedOntology, summedImports);
		}
		return summedImports;
	}
	
	public static String getIdIRI() {
		return IRI_BASE + IRI_ABBRIVATED_ID;
	}
	
	public static String getBaseIRI() {
		return IRI_BASE;
	}
}
