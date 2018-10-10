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
 * A representation of the model object '<em><b>Same Individual</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.SameIndividual#getSameIndividuals <em>Same Individuals</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getSameIndividual()
 * @model
 * @generated
 */
public interface SameIndividual extends Assertion {
	/**
	 * Returns the value of the '<em><b>Same Individuals</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.NamedIndividual}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Same Individuals</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Same Individuals</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getSameIndividual_SameIndividuals()
	 * @model lower="2" ordered="false"
	 * @generated
	 */
	EList<NamedIndividual> getSameIndividuals();

} // SameIndividual
