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
 * A representation of the model object '<em><b>Key For</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.KeyFor#getClassExpression <em>Class Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.KeyFor#getDataPropertyExpressions <em>Data Property Expressions</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.KeyFor#getObjectPropertyExpressions <em>Object Property Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getKeyFor()
 * @model
 * @generated
 */
public interface KeyFor extends ObjectAndDataPropertyAxiom {
	/**
	 * Returns the value of the '<em><b>Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Class Expression</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Class Expression</em>' reference.
	 * @see #setClassExpression(ClassExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getKeyFor_ClassExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ClassExpression getClassExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.KeyFor#getClassExpression <em>Class Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Class Expression</em>' reference.
	 * @see #getClassExpression()
	 * @generated
	 */
	void setClassExpression(ClassExpression value);

	/**
	 * Returns the value of the '<em><b>Data Property Expressions</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.DataPropertyExpression}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Property Expressions</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Data Property Expressions</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getKeyFor_DataPropertyExpressions()
	 * @model ordered="false"
	 * @generated
	 */
	EList<DataPropertyExpression> getDataPropertyExpressions();

	/**
	 * Returns the value of the '<em><b>Object Property Expressions</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.ObjectPropertyExpression}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Object Property Expressions</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Object Property Expressions</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getKeyFor_ObjectPropertyExpressions()
	 * @model ordered="false"
	 * @generated
	 */
	EList<ObjectPropertyExpression> getObjectPropertyExpressions();

} // KeyFor
