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
package carisma.modeltype.bpmn2.extended;


import org.eclipse.bpmn2.Task;
import org.eclipse.emf.common.util.EList;

import carisma.modeltype.bpmn2.extension.WorkItem;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Task</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extended.ExtendedTask#getWorkItem <em>Work Item</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.bpmn2.extended.ExtendedPackage#getExtendedTask()
 * @model
 * @generated
 */
public interface ExtendedTask extends Task {
	/**
	 * Returns the value of the '<em><b>Work Item</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.WorkItem}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Work Item</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Work Item</em>' reference list.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedPackage#getExtendedTask_WorkItem()
	 * @model
	 * @generated
	 */
	EList<WorkItem> getWorkItem();

} // ExtendedTask
