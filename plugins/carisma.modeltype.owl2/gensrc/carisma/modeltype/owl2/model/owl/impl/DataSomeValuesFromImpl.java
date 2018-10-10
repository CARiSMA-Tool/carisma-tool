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
import carisma.modeltype.owl2.model.owl.DataRange;
import carisma.modeltype.owl2.model.owl.DataSomeValuesFrom;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Data Some Values From</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DataSomeValuesFromImpl#getDataRange <em>Data Range</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DataSomeValuesFromImpl#getDataPropertyExpressions <em>Data Property Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DataSomeValuesFromImpl extends ClassExpressionImpl implements DataSomeValuesFrom {
	/**
	 * The cached value of the '{@link #getDataRange() <em>Data Range</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDataRange()
	 * @generated
	 * @ordered
	 */
	protected DataRange dataRange;

	/**
	 * The cached value of the '{@link #getDataPropertyExpressions() <em>Data Property Expressions</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDataPropertyExpressions()
	 * @generated
	 * @ordered
	 */
	protected DataPropertyExpression dataPropertyExpressions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DataSomeValuesFromImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.DATA_SOME_VALUES_FROM;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataRange getDataRange() {
		if (dataRange != null && dataRange.eIsProxy()) {
			InternalEObject oldDataRange = (InternalEObject)dataRange;
			dataRange = (DataRange)eResolveProxy(oldDataRange);
			if (dataRange != oldDataRange) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.DATA_SOME_VALUES_FROM__DATA_RANGE, oldDataRange, dataRange));
			}
		}
		return dataRange;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataRange basicGetDataRange() {
		return dataRange;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setDataRange(DataRange newDataRange) {
		DataRange oldDataRange = dataRange;
		dataRange = newDataRange;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.DATA_SOME_VALUES_FROM__DATA_RANGE, oldDataRange, dataRange));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyExpression getDataPropertyExpressions() {
		if (dataPropertyExpressions != null && dataPropertyExpressions.eIsProxy()) {
			InternalEObject oldDataPropertyExpressions = (InternalEObject)dataPropertyExpressions;
			dataPropertyExpressions = (DataPropertyExpression)eResolveProxy(oldDataPropertyExpressions);
			if (dataPropertyExpressions != oldDataPropertyExpressions) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.DATA_SOME_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS, oldDataPropertyExpressions, dataPropertyExpressions));
			}
		}
		return dataPropertyExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyExpression basicGetDataPropertyExpressions() {
		return dataPropertyExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setDataPropertyExpressions(DataPropertyExpression newDataPropertyExpressions) {
		DataPropertyExpression oldDataPropertyExpressions = dataPropertyExpressions;
		dataPropertyExpressions = newDataPropertyExpressions;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.DATA_SOME_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS, oldDataPropertyExpressions, dataPropertyExpressions));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.DATA_SOME_VALUES_FROM__DATA_RANGE:
				if (resolve) return getDataRange();
				return basicGetDataRange();
			case OwlPackage.DATA_SOME_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS:
				if (resolve) return getDataPropertyExpressions();
				return basicGetDataPropertyExpressions();
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
			case OwlPackage.DATA_SOME_VALUES_FROM__DATA_RANGE:
				setDataRange((DataRange)newValue);
				return;
			case OwlPackage.DATA_SOME_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS:
				setDataPropertyExpressions((DataPropertyExpression)newValue);
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
			case OwlPackage.DATA_SOME_VALUES_FROM__DATA_RANGE:
				setDataRange((DataRange)null);
				return;
			case OwlPackage.DATA_SOME_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS:
				setDataPropertyExpressions((DataPropertyExpression)null);
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
			case OwlPackage.DATA_SOME_VALUES_FROM__DATA_RANGE:
				return dataRange != null;
			case OwlPackage.DATA_SOME_VALUES_FROM__DATA_PROPERTY_EXPRESSIONS:
				return dataPropertyExpressions != null;
		}
		return super.eIsSet(featureID);
	}

} //DataSomeValuesFromImpl
