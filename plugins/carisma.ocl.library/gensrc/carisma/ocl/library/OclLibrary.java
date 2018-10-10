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
package carisma.ocl.library;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Ocl Library</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.ocl.library.OclLibrary#getOclExpressions <em>Ocl Expressions</em>}</li>
 *   <li>{@link carisma.ocl.library.OclLibrary#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.ocl.library.LibraryPackage#getOclLibrary()
 * @model
 * @generated
 */
public interface OclLibrary extends EObject {
	/**
	 * Returns the value of the '<em><b>Ocl Expressions</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.ocl.library.OclExpression}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ocl Expressions</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ocl Expressions</em>' containment reference list.
	 * @see carisma.ocl.library.LibraryPackage#getOclLibrary_OclExpressions()
	 * @model containment="true"
	 * @generated
	 */
	EList<OclExpression> getOclExpressions();

	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * The default value is <code>"MyLibrary"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see carisma.ocl.library.LibraryPackage#getOclLibrary_Name()
	 * @model default="MyLibrary"
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link carisma.ocl.library.OclLibrary#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

} // OclLibrary
