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

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

import carisma.modeltype.owl2.model.owl.ClassExpression;
import carisma.modeltype.owl2.model.owl.OwlPackage;
import carisma.modeltype.owl2.model.owl.SubClassOf;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Sub Class Of</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.SubClassOfImpl#getSubClassExpression <em>Sub Class Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.SubClassOfImpl#getSuperClassExpression <em>Super Class Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SubClassOfImpl extends ClassAxiomImpl implements SubClassOf {
	/**
	 * The cached value of the '{@link #getSubClassExpression() <em>Sub Class Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSubClassExpression()
	 * @generated
	 * @ordered
	 */
	protected ClassExpression subClassExpression;

	/**
	 * The cached value of the '{@link #getSuperClassExpression() <em>Super Class Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSuperClassExpression()
	 * @generated
	 * @ordered
	 */
	protected ClassExpression superClassExpression;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SubClassOfImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.SUB_CLASS_OF;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassExpression getSubClassExpression() {
		if (subClassExpression != null && subClassExpression.eIsProxy()) {
			InternalEObject oldSubClassExpression = (InternalEObject)subClassExpression;
			subClassExpression = (ClassExpression)eResolveProxy(oldSubClassExpression);
			if (subClassExpression != oldSubClassExpression) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.SUB_CLASS_OF__SUB_CLASS_EXPRESSION, oldSubClassExpression, subClassExpression));
			}
		}
		return subClassExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassExpression basicGetSubClassExpression() {
		return subClassExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSubClassExpression(ClassExpression newSubClassExpression) {
		ClassExpression oldSubClassExpression = subClassExpression;
		subClassExpression = newSubClassExpression;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.SUB_CLASS_OF__SUB_CLASS_EXPRESSION, oldSubClassExpression, subClassExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassExpression getSuperClassExpression() {
		if (superClassExpression != null && superClassExpression.eIsProxy()) {
			InternalEObject oldSuperClassExpression = (InternalEObject)superClassExpression;
			superClassExpression = (ClassExpression)eResolveProxy(oldSuperClassExpression);
			if (superClassExpression != oldSuperClassExpression) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.SUB_CLASS_OF__SUPER_CLASS_EXPRESSION, oldSuperClassExpression, superClassExpression));
			}
		}
		return superClassExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassExpression basicGetSuperClassExpression() {
		return superClassExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSuperClassExpression(ClassExpression newSuperClassExpression) {
		ClassExpression oldSuperClassExpression = superClassExpression;
		superClassExpression = newSuperClassExpression;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.SUB_CLASS_OF__SUPER_CLASS_EXPRESSION, oldSuperClassExpression, superClassExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.SUB_CLASS_OF__SUB_CLASS_EXPRESSION:
				if (resolve) return getSubClassExpression();
				return basicGetSubClassExpression();
			case OwlPackage.SUB_CLASS_OF__SUPER_CLASS_EXPRESSION:
				if (resolve) return getSuperClassExpression();
				return basicGetSuperClassExpression();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case OwlPackage.SUB_CLASS_OF__SUB_CLASS_EXPRESSION:
				setSubClassExpression((ClassExpression)newValue);
				return;
			case OwlPackage.SUB_CLASS_OF__SUPER_CLASS_EXPRESSION:
				setSuperClassExpression((ClassExpression)newValue);
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
			case OwlPackage.SUB_CLASS_OF__SUB_CLASS_EXPRESSION:
				setSubClassExpression((ClassExpression)null);
				return;
			case OwlPackage.SUB_CLASS_OF__SUPER_CLASS_EXPRESSION:
				setSuperClassExpression((ClassExpression)null);
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
			case OwlPackage.SUB_CLASS_OF__SUB_CLASS_EXPRESSION:
				return subClassExpression != null;
			case OwlPackage.SUB_CLASS_OF__SUPER_CLASS_EXPRESSION:
				return superClassExpression != null;
		}
		return super.eIsSet(featureID);
	}

} //SubClassOfImpl
