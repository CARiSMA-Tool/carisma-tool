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
 * A representation of the model object '<em><b>Different Individuals</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DifferentIndividuals#getDifferentIndividuals <em>Different Individuals</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDifferentIndividuals()
 * @model
 * @generated
 */
public interface DifferentIndividuals extends Assertion {
	/**
	 * Returns the value of the '<em><b>Different Individuals</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.NamedIndividual}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Different Individuals</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Different Individuals</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDifferentIndividuals_DifferentIndividuals()
	 * @model lower="2" ordered="false"
	 * @generated
	 */
	EList<NamedIndividual> getDifferentIndividuals();

} // DifferentIndividuals
