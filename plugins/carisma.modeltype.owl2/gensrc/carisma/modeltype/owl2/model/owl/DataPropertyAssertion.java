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
 * A representation of the model object '<em><b>Data Property Assertion</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getDataPropertyExpression <em>Data Property Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getTargetValue <em>Target Value</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getSourceIndividual <em>Source Individual</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataPropertyAssertion()
 * @model
 * @generated
 */
public interface DataPropertyAssertion extends Assertion {
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
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataPropertyAssertion_DataPropertyExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	DataPropertyExpression getDataPropertyExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getDataPropertyExpression <em>Data Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Data Property Expression</em>' reference.
	 * @see #getDataPropertyExpression()
	 * @generated
	 */
	void setDataPropertyExpression(DataPropertyExpression value);

	/**
	 * Returns the value of the '<em><b>Target Value</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Target Value</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Target Value</em>' reference.
	 * @see #setTargetValue(Constant)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataPropertyAssertion_TargetValue()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Constant getTargetValue();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getTargetValue <em>Target Value</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Target Value</em>' reference.
	 * @see #getTargetValue()
	 * @generated
	 */
	void setTargetValue(Constant value);

	/**
	 * Returns the value of the '<em><b>Source Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Source Individual</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Source Individual</em>' reference.
	 * @see #setSourceIndividual(Individual)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataPropertyAssertion_SourceIndividual()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Individual getSourceIndividual();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataPropertyAssertion#getSourceIndividual <em>Source Individual</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Source Individual</em>' reference.
	 * @see #getSourceIndividual()
	 * @generated
	 */
	void setSourceIndividual(Individual value);

} // DataPropertyAssertion
