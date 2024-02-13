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
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Root</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getTask <em>Task</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getWorkItem <em>Work Item</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getPerformer <em>Performer</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getRole <em>Role</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getLane <em>Lane</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getSelection <em>Selection</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getExtensionRoot()
 * @model
 * @generated
 */
public interface ExtensionRoot extends EObject {
	/**
	 * Returns the value of the '<em><b>Task</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Task}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Task</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Task</em>' containment reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getExtensionRoot_Task()
	 * @model containment="true"
	 * @generated
	 */
	EList<Task> getTask();

	/**
	 * Returns the value of the '<em><b>Work Item</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.WorkItem}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Work Item</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Work Item</em>' containment reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getExtensionRoot_WorkItem()
	 * @model containment="true"
	 * @generated
	 */
	EList<WorkItem> getWorkItem();

	/**
	 * Returns the value of the '<em><b>Performer</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Performer}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Performer</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Performer</em>' containment reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getExtensionRoot_Performer()
	 * @model containment="true"
	 * @generated
	 */
	EList<Performer> getPerformer();

	/**
	 * Returns the value of the '<em><b>Role</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Role}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Role</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Role</em>' containment reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getExtensionRoot_Role()
	 * @model containment="true"
	 * @generated
	 */
	EList<Role> getRole();

	/**
	 * Returns the value of the '<em><b>Lane</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Lane}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Lane</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Lane</em>' containment reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getExtensionRoot_Lane()
	 * @model containment="true"
	 * @generated
	 */
	EList<Lane> getLane();

	/**
	 * Returns the value of the '<em><b>Selection</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selection</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Selection</em>' containment reference.
	 * @see #setSelection(Selection)
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getExtensionRoot_Selection()
	 * @model containment="true"
	 * @generated
	 */
	Selection getSelection();

	/**
	 * Sets the value of the '{@link carisma.modeltype.bpmn2.extension.ExtensionRoot#getSelection <em>Selection</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Selection</em>' containment reference.
	 * @see #getSelection()
	 * @generated
	 */
	void setSelection(Selection value);

} // ExtensionRoot
