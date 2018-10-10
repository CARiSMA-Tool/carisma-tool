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

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Datatype Restriction</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DatatypeRestriction#getDatatype <em>Datatype</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DatatypeRestriction#getRestrictions <em>Restrictions</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDatatypeRestriction()
 * @model
 * @generated
 */
public interface DatatypeRestriction extends DataRange {
	/**
	 * Returns the value of the '<em><b>Datatype</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Datatype</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Datatype</em>' reference.
	 * @see #setDatatype(Datatype)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDatatypeRestriction_Datatype()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Datatype getDatatype();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DatatypeRestriction#getDatatype <em>Datatype</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Datatype</em>' reference.
	 * @see #getDatatype()
	 * @generated
	 */
	void setDatatype(Datatype value);

	/**
	 * Returns the value of the '<em><b>Restrictions</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.FacetConstantPair}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Restrictions</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Restrictions</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDatatypeRestriction_Restrictions()
	 * @model ordered="false"
	 * @generated
	 */
	EList<FacetConstantPair> getRestrictions();

} // DatatypeRestriction
