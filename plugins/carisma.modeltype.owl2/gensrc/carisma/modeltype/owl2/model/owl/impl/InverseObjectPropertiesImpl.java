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

import carisma.modeltype.owl2.model.owl.InverseObjectProperties;
import carisma.modeltype.owl2.model.owl.ObjectPropertyExpression;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Inverse Object Properties</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.InverseObjectPropertiesImpl#getInverseObjectProperties <em>Inverse Object Properties</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class InverseObjectPropertiesImpl extends ObjectPropertyAxiomImpl implements InverseObjectProperties {
	/**
	 * The cached value of the '{@link #getInverseObjectProperties() <em>Inverse Object Properties</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getInverseObjectProperties()
	 * @generated
	 * @ordered
	 */
	protected EList<ObjectPropertyExpression> inverseObjectProperties;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected InverseObjectPropertiesImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.INVERSE_OBJECT_PROPERTIES;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ObjectPropertyExpression> getInverseObjectProperties() {
		if (inverseObjectProperties == null) {
			inverseObjectProperties = new EObjectResolvingEList<ObjectPropertyExpression>(ObjectPropertyExpression.class, this, OwlPackage.INVERSE_OBJECT_PROPERTIES__INVERSE_OBJECT_PROPERTIES);
		}
		return inverseObjectProperties;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.INVERSE_OBJECT_PROPERTIES__INVERSE_OBJECT_PROPERTIES:
				return getInverseObjectProperties();
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
			case OwlPackage.INVERSE_OBJECT_PROPERTIES__INVERSE_OBJECT_PROPERTIES:
				getInverseObjectProperties().clear();
				getInverseObjectProperties().addAll((Collection<? extends ObjectPropertyExpression>)newValue);
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
			case OwlPackage.INVERSE_OBJECT_PROPERTIES__INVERSE_OBJECT_PROPERTIES:
				getInverseObjectProperties().clear();
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
			case OwlPackage.INVERSE_OBJECT_PROPERTIES__INVERSE_OBJECT_PROPERTIES:
				return inverseObjectProperties != null && !inverseObjectProperties.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //InverseObjectPropertiesImpl
