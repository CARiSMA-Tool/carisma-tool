/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.bpmn2.extension;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Role</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extension.Role#getName <em>Name</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.Role#getMember <em>Member</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.Role#getSuper <em>Super</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.Role#getSub <em>Sub</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.Role#getConflict <em>Conflict</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getRole()
 * @model
 * @generated
 */
public interface Role extends BaseElement {
	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getRole_Name()
	 * @model required="true"
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link carisma.modeltype.bpmn2.extension.Role#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Returns the value of the '<em><b>Member</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Performer}.
	 * It is bidirectional and its opposite is '{@link carisma.modeltype.bpmn2.extension.Performer#getRole <em>Role</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Member</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Member</em>' reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getRole_Member()
	 * @see carisma.modeltype.bpmn2.extension.Performer#getRole
	 * @model opposite="role"
	 * @generated
	 */
	EList<Performer> getMember();

	/**
	 * Returns the value of the '<em><b>Super</b></em>' reference.
	 * It is bidirectional and its opposite is '{@link carisma.modeltype.bpmn2.extension.Role#getSub <em>Sub</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Super</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Super</em>' reference.
	 * @see #setSuper(Role)
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getRole_Super()
	 * @see carisma.modeltype.bpmn2.extension.Role#getSub
	 * @model opposite="sub"
	 * @generated
	 */
	Role getSuper();

	/**
	 * Sets the value of the '{@link carisma.modeltype.bpmn2.extension.Role#getSuper <em>Super</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Super</em>' reference.
	 * @see #getSuper()
	 * @generated
	 */
	void setSuper(Role value);

	/**
	 * Returns the value of the '<em><b>Sub</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Role}.
	 * It is bidirectional and its opposite is '{@link carisma.modeltype.bpmn2.extension.Role#getSuper <em>Super</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Sub</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Sub</em>' reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getRole_Sub()
	 * @see carisma.modeltype.bpmn2.extension.Role#getSuper
	 * @model opposite="super"
	 * @generated
	 */
	EList<Role> getSub();

	/**
	 * Returns the value of the '<em><b>Conflict</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Role}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Conflict</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Conflict</em>' reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getRole_Conflict()
	 * @model
	 * @generated
	 */
	EList<Role> getConflict();

} // Role
