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


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Data Property Domain</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataPropertyDomain#getDomain <em>Domain</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataPropertyDomain#getDataPropertyExpression <em>Data Property Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataPropertyDomain()
 * @model
 * @generated
 */
public interface DataPropertyDomain extends DataPropertyAxiom {
	/**
	 * Returns the value of the '<em><b>Domain</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Domain</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Domain</em>' reference.
	 * @see #setDomain(ClassExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataPropertyDomain_Domain()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ClassExpression getDomain();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataPropertyDomain#getDomain <em>Domain</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Domain</em>' reference.
	 * @see #getDomain()
	 * @generated
	 */
	void setDomain(ClassExpression value);

	/**
	 * Returns the value of the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Property Expression</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Data Property Expression</em>' reference.
	 * @see #setDataPropertyExpression(DataPropertyExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataPropertyDomain_DataPropertyExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	DataPropertyExpression getDataPropertyExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataPropertyDomain#getDataPropertyExpression <em>Data Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Data Property Expression</em>' reference.
	 * @see #getDataPropertyExpression()
	 * @generated
	 */
	void setDataPropertyExpression(DataPropertyExpression value);

} // DataPropertyDomain
