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
 * A representation of the model object '<em><b>Equivalent Classes</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.EquivalentClasses#getEquivalentClassExpressions <em>Equivalent Class Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getEquivalentClasses()
 * @model
 * @generated
 */
public interface EquivalentClasses extends ClassAxiom {
	/**
	 * Returns the value of the '<em><b>Equivalent Class Expressions</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.ClassExpression}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Equivalent Class Expressions</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Equivalent Class Expressions</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getEquivalentClasses_EquivalentClassExpressions()
	 * @model lower="2" ordered="false"
	 * @generated
	 */
	EList<ClassExpression> getEquivalentClassExpressions();

} // EquivalentClasses
