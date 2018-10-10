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

import carisma.modeltype.owl2.model.owl.Constant;
import carisma.modeltype.owl2.model.owl.DataPropertyAssertion;
import carisma.modeltype.owl2.model.owl.DataPropertyExpression;
import carisma.modeltype.owl2.model.owl.Individual;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Data Property Assertion</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyAssertionImpl#getDataPropertyExpression <em>Data Property Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyAssertionImpl#getTargetValue <em>Target Value</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DataPropertyAssertionImpl#getSourceIndividual <em>Source Individual</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DataPropertyAssertionImpl extends AssertionImpl implements DataPropertyAssertion {
	/**
	 * The cached value of the '{@link #getDataPropertyExpression() <em>Data Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDataPropertyExpression()
	 * @generated
	 * @ordered
	 */
	protected DataPropertyExpression dataPropertyExpression;

	/**
	 * The cached value of the '{@link #getTargetValue() <em>Target Value</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTargetValue()
	 * @generated
	 * @ordered
	 */
	protected Constant targetValue;

	/**
	 * The cached value of the '{@link #getSourceIndividual() <em>Source Individual</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSourceIndividual()
	 * @generated
	 * @ordered
	 */
	protected Individual sourceIndividual;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DataPropertyAssertionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.DATA_PROPERTY_ASSERTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyExpression getDataPropertyExpression() {
		if (dataPropertyExpression != null && dataPropertyExpression.eIsProxy()) {
			InternalEObject oldDataPropertyExpression = (InternalEObject)dataPropertyExpression;
			dataPropertyExpression = (DataPropertyExpression)eResolveProxy(oldDataPropertyExpression);
			if (dataPropertyExpression != oldDataPropertyExpression) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION, oldDataPropertyExpression, dataPropertyExpression));
			}
		}
		return dataPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public DataPropertyExpression basicGetDataPropertyExpression() {
		return dataPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setDataPropertyExpression(DataPropertyExpression newDataPropertyExpression) {
		DataPropertyExpression oldDataPropertyExpression = dataPropertyExpression;
		dataPropertyExpression = newDataPropertyExpression;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION, oldDataPropertyExpression, dataPropertyExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Constant getTargetValue() {
		if (targetValue != null && targetValue.eIsProxy()) {
			InternalEObject oldTargetValue = (InternalEObject)targetValue;
			targetValue = (Constant)eResolveProxy(oldTargetValue);
			if (targetValue != oldTargetValue) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.DATA_PROPERTY_ASSERTION__TARGET_VALUE, oldTargetValue, targetValue));
			}
		}
		return targetValue;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Constant basicGetTargetValue() {
		return targetValue;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTargetValue(Constant newTargetValue) {
		Constant oldTargetValue = targetValue;
		targetValue = newTargetValue;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.DATA_PROPERTY_ASSERTION__TARGET_VALUE, oldTargetValue, targetValue));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Individual getSourceIndividual() {
		if (sourceIndividual != null && sourceIndividual.eIsProxy()) {
			InternalEObject oldSourceIndividual = (InternalEObject)sourceIndividual;
			sourceIndividual = (Individual)eResolveProxy(oldSourceIndividual);
			if (sourceIndividual != oldSourceIndividual) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL, oldSourceIndividual, sourceIndividual));
			}
		}
		return sourceIndividual;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Individual basicGetSourceIndividual() {
		return sourceIndividual;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSourceIndividual(Individual newSourceIndividual) {
		Individual oldSourceIndividual = sourceIndividual;
		sourceIndividual = newSourceIndividual;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL, oldSourceIndividual, sourceIndividual));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION:
				if (resolve) return getDataPropertyExpression();
				return basicGetDataPropertyExpression();
			case OwlPackage.DATA_PROPERTY_ASSERTION__TARGET_VALUE:
				if (resolve) return getTargetValue();
				return basicGetTargetValue();
			case OwlPackage.DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL:
				if (resolve) return getSourceIndividual();
				return basicGetSourceIndividual();
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
			case OwlPackage.DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION:
				setDataPropertyExpression((DataPropertyExpression)newValue);
				return;
			case OwlPackage.DATA_PROPERTY_ASSERTION__TARGET_VALUE:
				setTargetValue((Constant)newValue);
				return;
			case OwlPackage.DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL:
				setSourceIndividual((Individual)newValue);
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
			case OwlPackage.DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION:
				setDataPropertyExpression((DataPropertyExpression)null);
				return;
			case OwlPackage.DATA_PROPERTY_ASSERTION__TARGET_VALUE:
				setTargetValue((Constant)null);
				return;
			case OwlPackage.DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL:
				setSourceIndividual((Individual)null);
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
			case OwlPackage.DATA_PROPERTY_ASSERTION__DATA_PROPERTY_EXPRESSION:
				return dataPropertyExpression != null;
			case OwlPackage.DATA_PROPERTY_ASSERTION__TARGET_VALUE:
				return targetValue != null;
			case OwlPackage.DATA_PROPERTY_ASSERTION__SOURCE_INDIVIDUAL:
				return sourceIndividual != null;
		}
		return super.eIsSet(featureID);
	}

} //DataPropertyAssertionImpl
