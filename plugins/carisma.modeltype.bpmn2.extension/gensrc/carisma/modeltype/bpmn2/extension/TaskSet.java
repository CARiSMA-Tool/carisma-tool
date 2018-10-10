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
 * A representation of the model object '<em><b>Task Set</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extension.TaskSet#getName <em>Name</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.TaskSet#getSelectedTasks <em>Selected Tasks</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getTaskSet()
 * @model
 * @generated
 */
public interface TaskSet extends BaseElement {
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
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getTaskSet_Name()
	 * @model required="true"
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link carisma.modeltype.bpmn2.extension.TaskSet#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Returns the value of the '<em><b>Selected Tasks</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Task}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Selected Tasks</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Selected Tasks</em>' reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getTaskSet_SelectedTasks()
	 * @model lower="2"
	 * @generated
	 */
	EList<Task> getSelectedTasks();

} // TaskSet
