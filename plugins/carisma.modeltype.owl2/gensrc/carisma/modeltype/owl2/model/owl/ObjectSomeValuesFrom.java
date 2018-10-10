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
 * A representation of the model object '<em><b>Object Some Values From</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom#getClassExpression <em>Class Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom#getObjectPropertyExpression <em>Object Property Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getObjectSomeValuesFrom()
 * @model
 * @generated
 */
public interface ObjectSomeValuesFrom extends ClassExpression {
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
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getObjectSomeValuesFrom_ClassExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ClassExpression getClassExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom#getClassExpression <em>Class Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Class Expression</em>' reference.
	 * @see #getClassExpression()
	 * @generated
	 */
	void setClassExpression(ClassExpression value);

	/**
	 * Returns the value of the '<em><b>Object Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Object Property Expression</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Object Property Expression</em>' reference.
	 * @see #setObjectPropertyExpression(ObjectPropertyExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getObjectSomeValuesFrom_ObjectPropertyExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ObjectPropertyExpression getObjectPropertyExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.ObjectSomeValuesFrom#getObjectPropertyExpression <em>Object Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Object Property Expression</em>' reference.
	 * @see #getObjectPropertyExpression()
	 * @generated
	 */
	void setObjectPropertyExpression(ObjectPropertyExpression value);

} // ObjectSomeValuesFrom
