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
 * A representation of the model object '<em><b>Data One Of</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataOneOf#getConstants <em>Constants</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataOneOf()
 * @model
 * @generated
 */
public interface DataOneOf extends DataRange {
	/**
	 * Returns the value of the '<em><b>Constants</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.Constant}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Constants</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Constants</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataOneOf_Constants()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Constant> getConstants();

} // DataOneOf
