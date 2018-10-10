/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.owl2.model.owl;

import java.util.Map;

import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Ontology</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Ontology#getOntologyAnnotations <em>Ontology Annotations</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Ontology#getAxioms <em>Axioms</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Ontology#getImportedOntologies <em>Imported Ontologies</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Ontology#getOntologyURI <em>Ontology URI</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Ontology#getVersionURI <em>Version URI</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Ontology#getContainer <em>Container</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Ontology#getOntologyId <em>Ontology Id</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getOntology()
 * @model
 * @generated
 */
public interface Ontology extends EObject {
	/**
	 * Returns the value of the '<em><b>Ontology Annotations</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.Annotation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ontology Annotations</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ontology Annotations</em>' containment reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getOntology_OntologyAnnotations()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	EList<Annotation> getOntologyAnnotations();

	/**
	 * Returns the value of the '<em><b>Axioms</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.Axiom}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Axioms</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Axioms</em>' containment reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getOntology_Axioms()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	EList<Axiom> getAxioms();

	/**
	 * Returns the value of the '<em><b>Imported Ontologies</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.Ontology}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Imported Ontologies</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Imported Ontologies</em>' containment reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getOntology_ImportedOntologies()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	EList<Ontology> getImportedOntologies();

	/**
	 * Returns the value of the '<em><b>Ontology URI</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ontology URI</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ontology URI</em>' containment reference.
	 * @see #setOntologyURI(URI)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getOntology_OntologyURI()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	URI getOntologyURI();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.Ontology#getOntologyURI <em>Ontology URI</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ontology URI</em>' containment reference.
	 * @see #getOntologyURI()
	 * @generated
	 */
	void setOntologyURI(URI value);

	/**
	 * Returns the value of the '<em><b>Version URI</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Version URI</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Version URI</em>' containment reference.
	 * @see #setVersionURI(URI)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getOntology_VersionURI()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	URI getVersionURI();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.Ontology#getVersionURI <em>Version URI</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Version URI</em>' containment reference.
	 * @see #getVersionURI()
	 * @generated
	 */
	void setVersionURI(URI value);

	/**
	 * Returns the value of the '<em><b>Container</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecore.EObject}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Container</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Container</em>' containment reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getOntology_Container()
	 * @model containment="true"
	 * @generated
	 */
	EList<EObject> getContainer();

	/**
	 * Returns the value of the '<em><b>Ontology Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ontology Id</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ontology Id</em>' attribute.
	 * @see #setOntologyId(String)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getOntology_OntologyId()
	 * @model id="true" required="true"
	 * @generated
	 */
	String getOntologyId();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.Ontology#getOntologyId <em>Ontology Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ontology Id</em>' attribute.
	 * @see #getOntologyId()
	 * @generated
	 */
	void setOntologyId(String value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * self.versionURI != 0 implies self.ontologyURI != 0
	 * <!-- end-model-doc -->
	 * @model
	 * @generated
	 */
	boolean versionURIrequiresontologyURItobespecified(DiagnosticChain diagnostics, Map context);

} // Ontology
