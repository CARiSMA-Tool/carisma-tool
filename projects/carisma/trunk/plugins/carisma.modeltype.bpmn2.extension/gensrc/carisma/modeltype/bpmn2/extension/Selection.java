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
 * A representation of the model object '<em><b>Selection</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extension.Selection#getTaskSet <em>Task Set</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getSelection()
 * @model
 * @generated
 */
public interface Selection extends BaseElement {
	/**
	 * Returns the value of the '<em><b>Task Set</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.TaskSet}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Task Set</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Task Set</em>' containment reference list.
	 * @see carisma.modeltype.bpmn2.extension.ExtensionPackage#getSelection_TaskSet()
	 * @model containment="true"
	 * @generated
	 */
	EList<TaskSet> getTaskSet();

} // Selection
