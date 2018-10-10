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

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import carisma.modeltype.owl2.model.owl.ClassExpression;
import carisma.modeltype.owl2.model.owl.DisjointUnion;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Disjoint Union</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DisjointUnionImpl#getUnionClass <em>Union Class</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DisjointUnionImpl#getDisjointClassExpressions <em>Disjoint Class Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DisjointUnionImpl extends ClassAxiomImpl implements DisjointUnion {
	/**
	 * The cached value of the '{@link #getUnionClass() <em>Union Class</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getUnionClass()
	 * @generated
	 * @ordered
	 */
	protected carisma.modeltype.owl2.model.owl.Class unionClass;

	/**
	 * The cached value of the '{@link #getDisjointClassExpressions() <em>Disjoint Class Expressions</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDisjointClassExpressions()
	 * @generated
	 * @ordered
	 */
	protected EList<ClassExpression> disjointClassExpressions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DisjointUnionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.DISJOINT_UNION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public carisma.modeltype.owl2.model.owl.Class getUnionClass() {
		if (unionClass != null && unionClass.eIsProxy()) {
			InternalEObject oldUnionClass = (InternalEObject)unionClass;
			unionClass = (carisma.modeltype.owl2.model.owl.Class)eResolveProxy(oldUnionClass);
			if (unionClass != oldUnionClass) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.DISJOINT_UNION__UNION_CLASS, oldUnionClass, unionClass));
			}
		}
		return unionClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public carisma.modeltype.owl2.model.owl.Class basicGetUnionClass() {
		return unionClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setUnionClass(carisma.modeltype.owl2.model.owl.Class newUnionClass) {
		carisma.modeltype.owl2.model.owl.Class oldUnionClass = unionClass;
		unionClass = newUnionClass;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.DISJOINT_UNION__UNION_CLASS, oldUnionClass, unionClass));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ClassExpression> getDisjointClassExpressions() {
		if (disjointClassExpressions == null) {
			disjointClassExpressions = new EObjectResolvingEList<ClassExpression>(ClassExpression.class, this, OwlPackage.DISJOINT_UNION__DISJOINT_CLASS_EXPRESSIONS);
		}
		return disjointClassExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.DISJOINT_UNION__UNION_CLASS:
				if (resolve) return getUnionClass();
				return basicGetUnionClass();
			case OwlPackage.DISJOINT_UNION__DISJOINT_CLASS_EXPRESSIONS:
				return getDisjointClassExpressions();
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
			case OwlPackage.DISJOINT_UNION__UNION_CLASS:
				setUnionClass((carisma.modeltype.owl2.model.owl.Class)newValue);
				return;
			case OwlPackage.DISJOINT_UNION__DISJOINT_CLASS_EXPRESSIONS:
				getDisjointClassExpressions().clear();
				getDisjointClassExpressions().addAll((Collection<? extends ClassExpression>)newValue);
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
			case OwlPackage.DISJOINT_UNION__UNION_CLASS:
				setUnionClass((carisma.modeltype.owl2.model.owl.Class)null);
				return;
			case OwlPackage.DISJOINT_UNION__DISJOINT_CLASS_EXPRESSIONS:
				getDisjointClassExpressions().clear();
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
			case OwlPackage.DISJOINT_UNION__UNION_CLASS:
				return unionClass != null;
			case OwlPackage.DISJOINT_UNION__DISJOINT_CLASS_EXPRESSIONS:
				return disjointClassExpressions != null && !disjointClassExpressions.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //DisjointUnionImpl
