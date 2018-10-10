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
package carisma.modeltype.owl2.model.owl.impl;

import java.util.Collection;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import carisma.modeltype.owl2.model.owl.ClassExpression;
import carisma.modeltype.owl2.model.owl.ObjectIntersectionOf;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Object Intersection Of</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.ObjectIntersectionOfImpl#getClassExpressions <em>Class Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ObjectIntersectionOfImpl extends ClassExpressionImpl implements ObjectIntersectionOf {
	/**
	 * The cached value of the '{@link #getClassExpressions() <em>Class Expressions</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getClassExpressions()
	 * @generated
	 * @ordered
	 */
	protected EList<ClassExpression> classExpressions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ObjectIntersectionOfImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.OBJECT_INTERSECTION_OF;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ClassExpression> getClassExpressions() {
		if (classExpressions == null) {
			classExpressions = new EObjectResolvingEList<ClassExpression>(ClassExpression.class, this, OwlPackage.OBJECT_INTERSECTION_OF__CLASS_EXPRESSIONS);
		}
		return classExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.OBJECT_INTERSECTION_OF__CLASS_EXPRESSIONS:
				return getClassExpressions();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case OwlPackage.OBJECT_INTERSECTION_OF__CLASS_EXPRESSIONS:
				getClassExpressions().clear();
				getClassExpressions().addAll((Collection<? extends ClassExpression>)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case OwlPackage.OBJECT_INTERSECTION_OF__CLASS_EXPRESSIONS:
				getClassExpressions().clear();
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case OwlPackage.OBJECT_INTERSECTION_OF__CLASS_EXPRESSIONS:
				return classExpressions != null && !classExpressions.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //ObjectIntersectionOfImpl
