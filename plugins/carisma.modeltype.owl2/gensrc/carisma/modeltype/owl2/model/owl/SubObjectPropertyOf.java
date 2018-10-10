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
 * A representation of the model object '<em><b>Sub Object Property Of</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.SubObjectPropertyOf#getSuperObjectPropertyExpression <em>Super Object Property Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.SubObjectPropertyOf#getSubObjectPropertyExpressions <em>Sub Object Property Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getSubObjectPropertyOf()
 * @model
 * @generated
 */
public interface SubObjectPropertyOf extends ObjectPropertyAxiom {
	/**
	 * Returns the value of the '<em><b>Super Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Super Object Property Expression</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Super Object Property Expression</em>' reference.
	 * @see #setSuperObjectPropertyExpression(ObjectPropertyExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getSubObjectPropertyOf_SuperObjectPropertyExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ObjectPropertyExpression getSuperObjectPropertyExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.SubObjectPropertyOf#getSuperObjectPropertyExpression <em>Super Object Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Super Object Property Expression</em>' reference.
	 * @see #getSuperObjectPropertyExpression()
	 * @generated
	 */
	void setSuperObjectPropertyExpression(ObjectPropertyExpression value);

	/**
	 * Returns the value of the '<em><b>Sub Object Property Expressions</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Sub Object Property Expressions</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Sub Object Property Expressions</em>' reference.
	 * @see #setSubObjectPropertyExpressions(ObjectPropertyExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getSubObjectPropertyOf_SubObjectPropertyExpressions()
	 * @model required="true"
	 * @generated
	 */
	ObjectPropertyExpression getSubObjectPropertyExpressions();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.SubObjectPropertyOf#getSubObjectPropertyExpressions <em>Sub Object Property Expressions</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Sub Object Property Expressions</em>' reference.
	 * @see #getSubObjectPropertyExpressions()
	 * @generated
	 */
	void setSubObjectPropertyExpressions(ObjectPropertyExpression value);

} // SubObjectPropertyOf
