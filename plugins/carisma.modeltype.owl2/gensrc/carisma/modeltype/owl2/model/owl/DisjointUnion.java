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
 * A representation of the model object '<em><b>Disjoint Union</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DisjointUnion#getUnionClass <em>Union Class</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DisjointUnion#getDisjointClassExpressions <em>Disjoint Class Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDisjointUnion()
 * @model
 * @generated
 */
public interface DisjointUnion extends ClassAxiom {
	/**
	 * Returns the value of the '<em><b>Union Class</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Union Class</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Union Class</em>' reference.
	 * @see #setUnionClass(carisma.modeltype.owl2.model.owl.Class)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDisjointUnion_UnionClass()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	carisma.modeltype.owl2.model.owl.Class getUnionClass();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DisjointUnion#getUnionClass <em>Union Class</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Union Class</em>' reference.
	 * @see #getUnionClass()
	 * @generated
	 */
	void setUnionClass(carisma.modeltype.owl2.model.owl.Class value);

	/**
	 * Returns the value of the '<em><b>Disjoint Class Expressions</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.ClassExpression}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Disjoint Class Expressions</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Disjoint Class Expressions</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDisjointUnion_DisjointClassExpressions()
	 * @model lower="2" ordered="false"
	 * @generated
	 */
	EList<ClassExpression> getDisjointClassExpressions();

} // DisjointUnion
