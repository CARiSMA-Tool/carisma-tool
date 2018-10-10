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
 * A representation of the model object '<em><b>Sub Class Of</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.SubClassOf#getSubClassExpression <em>Sub Class Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.SubClassOf#getSuperClassExpression <em>Super Class Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getSubClassOf()
 * @model
 * @generated
 */
public interface SubClassOf extends ClassAxiom {
	/**
	 * Returns the value of the '<em><b>Sub Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Sub Class Expression</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Sub Class Expression</em>' reference.
	 * @see #setSubClassExpression(ClassExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getSubClassOf_SubClassExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ClassExpression getSubClassExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.SubClassOf#getSubClassExpression <em>Sub Class Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Sub Class Expression</em>' reference.
	 * @see #getSubClassExpression()
	 * @generated
	 */
	void setSubClassExpression(ClassExpression value);

	/**
	 * Returns the value of the '<em><b>Super Class Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Super Class Expression</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Super Class Expression</em>' reference.
	 * @see #setSuperClassExpression(ClassExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getSubClassOf_SuperClassExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ClassExpression getSuperClassExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.SubClassOf#getSuperClassExpression <em>Super Class Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Super Class Expression</em>' reference.
	 * @see #getSuperClassExpression()
	 * @generated
	 */
	void setSuperClassExpression(ClassExpression value);

} // SubClassOf
