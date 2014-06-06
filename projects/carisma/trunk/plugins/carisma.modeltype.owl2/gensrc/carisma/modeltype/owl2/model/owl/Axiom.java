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

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Axiom</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Axiom#getAxiomAnnotations <em>Axiom Annotations</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Axiom#getAxiomId <em>Axiom Id</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getAxiom()
 * @model abstract="true"
 * @generated
 */
public interface Axiom extends EObject {
	/**
	 * Returns the value of the '<em><b>Axiom Annotations</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.Annotation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Axiom Annotations</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Axiom Annotations</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getAxiom_AxiomAnnotations()
	 * @model ordered="false"
	 * @generated
	 */
	EList<Annotation> getAxiomAnnotations();

	/**
	 * Returns the value of the '<em><b>Axiom Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Axiom Id</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Axiom Id</em>' attribute.
	 * @see #setAxiomId(String)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getAxiom_AxiomId()
	 * @model id="true" required="true"
	 * @generated
	 */
	String getAxiomId();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.Axiom#getAxiomId <em>Axiom Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Axiom Id</em>' attribute.
	 * @see #getAxiomId()
	 * @generated
	 */
	void setAxiomId(String value);

} // Axiom
