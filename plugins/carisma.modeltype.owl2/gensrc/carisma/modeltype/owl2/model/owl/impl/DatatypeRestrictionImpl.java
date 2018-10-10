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

import carisma.modeltype.owl2.model.owl.Datatype;
import carisma.modeltype.owl2.model.owl.DatatypeRestriction;
import carisma.modeltype.owl2.model.owl.FacetConstantPair;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Datatype Restriction</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DatatypeRestrictionImpl#getDatatype <em>Datatype</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DatatypeRestrictionImpl#getRestrictions <em>Restrictions</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DatatypeRestrictionImpl extends DataRangeImpl implements DatatypeRestriction {
	/**
	 * The cached value of the '{@link #getDatatype() <em>Datatype</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDatatype()
	 * @generated
	 * @ordered
	 */
	protected Datatype datatype;

	/**
	 * The cached value of the '{@link #getRestrictions() <em>Restrictions</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRestrictions()
	 * @generated
	 * @ordered
	 */
	protected EList<FacetConstantPair> restrictions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DatatypeRestrictionImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.DATATYPE_RESTRICTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Datatype getDatatype() {
		if (datatype != null && datatype.eIsProxy()) {
			InternalEObject oldDatatype = (InternalEObject)datatype;
			datatype = (Datatype)eResolveProxy(oldDatatype);
			if (datatype != oldDatatype) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.DATATYPE_RESTRICTION__DATATYPE, oldDatatype, datatype));
			}
		}
		return datatype;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Datatype basicGetDatatype() {
		return datatype;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setDatatype(Datatype newDatatype) {
		Datatype oldDatatype = datatype;
		datatype = newDatatype;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.DATATYPE_RESTRICTION__DATATYPE, oldDatatype, datatype));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<FacetConstantPair> getRestrictions() {
		if (restrictions == null) {
			restrictions = new EObjectResolvingEList<FacetConstantPair>(FacetConstantPair.class, this, OwlPackage.DATATYPE_RESTRICTION__RESTRICTIONS);
		}
		return restrictions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.DATATYPE_RESTRICTION__DATATYPE:
				if (resolve) return getDatatype();
				return basicGetDatatype();
			case OwlPackage.DATATYPE_RESTRICTION__RESTRICTIONS:
				return getRestrictions();
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
			case OwlPackage.DATATYPE_RESTRICTION__DATATYPE:
				setDatatype((Datatype)newValue);
				return;
			case OwlPackage.DATATYPE_RESTRICTION__RESTRICTIONS:
				getRestrictions().clear();
				getRestrictions().addAll((Collection<? extends FacetConstantPair>)newValue);
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
			case OwlPackage.DATATYPE_RESTRICTION__DATATYPE:
				setDatatype((Datatype)null);
				return;
			case OwlPackage.DATATYPE_RESTRICTION__RESTRICTIONS:
				getRestrictions().clear();
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
			case OwlPackage.DATATYPE_RESTRICTION__DATATYPE:
				return datatype != null;
			case OwlPackage.DATATYPE_RESTRICTION__RESTRICTIONS:
				return restrictions != null && !restrictions.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //DatatypeRestrictionImpl
