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

import carisma.modeltype.owl2.model.owl.DataPropertyExpression;
import carisma.modeltype.owl2.model.owl.OwlPackage;
import carisma.modeltype.owl2.model.owl.SubDataPropertyOf;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Sub Data Property Of</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.SubDataPropertyOfImpl#getSuperDataPropertyExpression <em>Super Data Property Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.SubDataPropertyOfImpl#getSubDataPropertyExpression <em>Sub Data Property Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SubDataPropertyOfImpl extends DataPropertyAxiomImpl implements SubDataPropertyOf {
	/**
	 * The cached value of the '{@link #getSuperDataPropertyExpression() <em>Super Data Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSuperDataPropertyExpression()
	 * @generated
	 * @ordered
	 */
	protected DataPropertyExpression superDataPropertyExpression;

	/**
	 * The cached value of the '{@link #getSubDataPropertyExpression() <em>Sub Data Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSubDataPropertyExpression()
	 * @generated
	 * @ordered
	 */
	protected DataPropertyExpression subDataPropertyExpression;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SubDataPropertyOfImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.SUB_DATA_PROPERTY_OF;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyExpression getSuperDataPropertyExpression() {
		if (superDataPropertyExpression != null && superDataPropertyExpression.eIsProxy()) {
			InternalEObject oldSuperDataPropertyExpression = (InternalEObject)superDataPropertyExpression;
			superDataPropertyExpression = (DataPropertyExpression)eResolveProxy(oldSuperDataPropertyExpression);
			if (superDataPropertyExpression != oldSuperDataPropertyExpression) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.SUB_DATA_PROPERTY_OF__SUPER_DATA_PROPERTY_EXPRESSION, oldSuperDataPropertyExpression, superDataPropertyExpression));
			}
		}
		return superDataPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyExpression basicGetSuperDataPropertyExpression() {
		return superDataPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSuperDataPropertyExpression(DataPropertyExpression newSuperDataPropertyExpression) {
		DataPropertyExpression oldSuperDataPropertyExpression = superDataPropertyExpression;
		superDataPropertyExpression = newSuperDataPropertyExpression;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.SUB_DATA_PROPERTY_OF__SUPER_DATA_PROPERTY_EXPRESSION, oldSuperDataPropertyExpression, superDataPropertyExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyExpression getSubDataPropertyExpression() {
		if (subDataPropertyExpression != null && subDataPropertyExpression.eIsProxy()) {
			InternalEObject oldSubDataPropertyExpression = (InternalEObject)subDataPropertyExpression;
			subDataPropertyExpression = (DataPropertyExpression)eResolveProxy(oldSubDataPropertyExpression);
			if (subDataPropertyExpression != oldSubDataPropertyExpression) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.SUB_DATA_PROPERTY_OF__SUB_DATA_PROPERTY_EXPRESSION, oldSubDataPropertyExpression, subDataPropertyExpression));
			}
		}
		return subDataPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyExpression basicGetSubDataPropertyExpression() {
		return subDataPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSubDataPropertyExpression(DataPropertyExpression newSubDataPropertyExpression) {
		DataPropertyExpression oldSubDataPropertyExpression = subDataPropertyExpression;
		subDataPropertyExpression = newSubDataPropertyExpression;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.SUB_DATA_PROPERTY_OF__SUB_DATA_PROPERTY_EXPRESSION, oldSubDataPropertyExpression, subDataPropertyExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.SUB_DATA_PROPERTY_OF__SUPER_DATA_PROPERTY_EXPRESSION:
				if (resolve) return getSuperDataPropertyExpression();
				return basicGetSuperDataPropertyExpression();
			case OwlPackage.SUB_DATA_PROPERTY_OF__SUB_DATA_PROPERTY_EXPRESSION:
				if (resolve) return getSubDataPropertyExpression();
				return basicGetSubDataPropertyExpression();
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
			case OwlPackage.SUB_DATA_PROPERTY_OF__SUPER_DATA_PROPERTY_EXPRESSION:
				setSuperDataPropertyExpression((DataPropertyExpression)newValue);
				return;
			case OwlPackage.SUB_DATA_PROPERTY_OF__SUB_DATA_PROPERTY_EXPRESSION:
				setSubDataPropertyExpression((DataPropertyExpression)newValue);
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
			case OwlPackage.SUB_DATA_PROPERTY_OF__SUPER_DATA_PROPERTY_EXPRESSION:
				setSuperDataPropertyExpression((DataPropertyExpression)null);
				return;
			case OwlPackage.SUB_DATA_PROPERTY_OF__SUB_DATA_PROPERTY_EXPRESSION:
				setSubDataPropertyExpression((DataPropertyExpression)null);
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
			case OwlPackage.SUB_DATA_PROPERTY_OF__SUPER_DATA_PROPERTY_EXPRESSION:
				return superDataPropertyExpression != null;
			case OwlPackage.SUB_DATA_PROPERTY_OF__SUB_DATA_PROPERTY_EXPRESSION:
				return subDataPropertyExpression != null;
		}
		return super.eIsSet(featureID);
	}

} //SubDataPropertyOfImpl
