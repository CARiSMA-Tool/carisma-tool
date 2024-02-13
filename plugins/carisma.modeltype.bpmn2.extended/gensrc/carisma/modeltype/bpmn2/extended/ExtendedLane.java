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


import org.eclipse.bpmn2.Lane;
import org.eclipse.emf.common.util.EList;

import carisma.modeltype.bpmn2.extension.Role;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Lane</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extended.ExtendedLane#getRole <em>Role</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.bpmn2.extended.ExtendedPackage#getExtendedLane()
 * @model
 * @generated
 */
public interface ExtendedLane extends Lane {
	/**
	 * Returns the value of the '<em><b>Role</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.bpmn2.extension.Role}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Role</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Role</em>' reference list.
	 * @see carisma.modeltype.bpmn2.extended.ExtendedPackage#getExtendedLane_Role()
	 * @model
	 * @generated
	 */
	EList<Role> getRole();

} // ExtendedLane
