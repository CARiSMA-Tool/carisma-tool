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

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see carisma.ocl.library.LibraryFactory
 * @model kind="package"
 * @generated
 */
public interface LibraryPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "library";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http:///carisma/ocl.ecore";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "library";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	LibraryPackage eINSTANCE = carisma.ocl.library.impl.LibraryPackageImpl.init();

	/**
	 * The meta object id for the '{@link carisma.ocl.library.impl.OclExpressionImpl <em>Ocl Expression</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.ocl.library.impl.OclExpressionImpl
	 * @see carisma.ocl.library.impl.LibraryPackageImpl#getOclExpression()
	 * @generated
	 */
	int OCL_EXPRESSION = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OCL_EXPRESSION__NAME = 0;

	/**
	 * The feature id for the '<em><b>Description</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OCL_EXPRESSION__DESCRIPTION = 1;

	/**
	 * The feature id for the '<em><b>Query</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OCL_EXPRESSION__QUERY = 2;

	/**
	 * The feature id for the '<em><b>Context</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OCL_EXPRESSION__CONTEXT = 3;

	/**
	 * The number of structural features of the '<em>Ocl Expression</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OCL_EXPRESSION_FEATURE_COUNT = 4;

	/**
	 * The meta object id for the '{@link carisma.ocl.library.impl.OclLibraryImpl <em>Ocl Library</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see carisma.ocl.library.impl.OclLibraryImpl
	 * @see carisma.ocl.library.impl.LibraryPackageImpl#getOclLibrary()
	 * @generated
	 */
	int OCL_LIBRARY = 1;

	/**
	 * The feature id for the '<em><b>Ocl Expressions</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OCL_LIBRARY__OCL_EXPRESSIONS = 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OCL_LIBRARY__NAME = 1;

	/**
	 * The number of structural features of the '<em>Ocl Library</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int OCL_LIBRARY_FEATURE_COUNT = 2;


	/**
	 * Returns the meta object for class '{@link carisma.ocl.library.OclExpression <em>Ocl Expression</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Ocl Expression</em>'.
	 * @see carisma.ocl.library.OclExpression
	 * @generated
	 */
	EClass getOclExpression();

	/**
	 * Returns the meta object for the attribute '{@link carisma.ocl.library.OclExpression#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.ocl.library.OclExpression#getName()
	 * @see #getOclExpression()
	 * @generated
	 */
	EAttribute getOclExpression_Name();

	/**
	 * Returns the meta object for the attribute '{@link carisma.ocl.library.OclExpression#getDescription <em>Description</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Description</em>'.
	 * @see carisma.ocl.library.OclExpression#getDescription()
	 * @see #getOclExpression()
	 * @generated
	 */
	EAttribute getOclExpression_Description();

	/**
	 * Returns the meta object for the attribute '{@link carisma.ocl.library.OclExpression#getQuery <em>Query</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Query</em>'.
	 * @see carisma.ocl.library.OclExpression#getQuery()
	 * @see #getOclExpression()
	 * @generated
	 */
	EAttribute getOclExpression_Query();

	/**
	 * Returns the meta object for the attribute '{@link carisma.ocl.library.OclExpression#getContext <em>Context</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Context</em>'.
	 * @see carisma.ocl.library.OclExpression#getContext()
	 * @see #getOclExpression()
	 * @generated
	 */
	EAttribute getOclExpression_Context();

	/**
	 * Returns the meta object for class '{@link carisma.ocl.library.OclLibrary <em>Ocl Library</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Ocl Library</em>'.
	 * @see carisma.ocl.library.OclLibrary
	 * @generated
	 */
	EClass getOclLibrary();

	/**
	 * Returns the meta object for the containment reference list '{@link carisma.ocl.library.OclLibrary#getOclExpressions <em>Ocl Expressions</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Ocl Expressions</em>'.
	 * @see carisma.ocl.library.OclLibrary#getOclExpressions()
	 * @see #getOclLibrary()
	 * @generated
	 */
	EReference getOclLibrary_OclExpressions();

	/**
	 * Returns the meta object for the attribute '{@link carisma.ocl.library.OclLibrary#getName <em>Name</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Name</em>'.
	 * @see carisma.ocl.library.OclLibrary#getName()
	 * @see #getOclLibrary()
	 * @generated
	 */
	EAttribute getOclLibrary_Name();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	LibraryFactory getLibraryFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link carisma.ocl.library.impl.OclExpressionImpl <em>Ocl Expression</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.ocl.library.impl.OclExpressionImpl
		 * @see carisma.ocl.library.impl.LibraryPackageImpl#getOclExpression()
		 * @generated
		 */
		EClass OCL_EXPRESSION = eINSTANCE.getOclExpression();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OCL_EXPRESSION__NAME = eINSTANCE.getOclExpression_Name();

		/**
		 * The meta object literal for the '<em><b>Description</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OCL_EXPRESSION__DESCRIPTION = eINSTANCE.getOclExpression_Description();

		/**
		 * The meta object literal for the '<em><b>Query</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OCL_EXPRESSION__QUERY = eINSTANCE.getOclExpression_Query();

		/**
		 * The meta object literal for the '<em><b>Context</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OCL_EXPRESSION__CONTEXT = eINSTANCE.getOclExpression_Context();

		/**
		 * The meta object literal for the '{@link carisma.ocl.library.impl.OclLibraryImpl <em>Ocl Library</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see carisma.ocl.library.impl.OclLibraryImpl
		 * @see carisma.ocl.library.impl.LibraryPackageImpl#getOclLibrary()
		 * @generated
		 */
		EClass OCL_LIBRARY = eINSTANCE.getOclLibrary();

		/**
		 * The meta object literal for the '<em><b>Ocl Expressions</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference OCL_LIBRARY__OCL_EXPRESSIONS = eINSTANCE.getOclLibrary_OclExpressions();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute OCL_LIBRARY__NAME = eINSTANCE.getOclLibrary_Name();

	}

} //LibraryPackage
