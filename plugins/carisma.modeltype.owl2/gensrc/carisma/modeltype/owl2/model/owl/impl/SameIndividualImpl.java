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

import carisma.modeltype.owl2.model.owl.NamedIndividual;
import carisma.modeltype.owl2.model.owl.OwlPackage;
import carisma.modeltype.owl2.model.owl.SameIndividual;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Same Individual</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.SameIndividualImpl#getSameIndividuals <em>Same Individuals</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SameIndividualImpl extends AssertionImpl implements SameIndividual {
	/**
	 * The cached value of the '{@link #getSameIndividuals() <em>Same Individuals</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSameIndividuals()
	 * @generated
	 * @ordered
	 */
	protected EList<NamedIndividual> sameIndividuals;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SameIndividualImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.SAME_INDIVIDUAL;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<NamedIndividual> getSameIndividuals() {
		if (sameIndividuals == null) {
			sameIndividuals = new EObjectResolvingEList<NamedIndividual>(NamedIndividual.class, this, OwlPackage.SAME_INDIVIDUAL__SAME_INDIVIDUALS);
		}
		return sameIndividuals;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.SAME_INDIVIDUAL__SAME_INDIVIDUALS:
				return getSameIndividuals();
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
			case OwlPackage.SAME_INDIVIDUAL__SAME_INDIVIDUALS:
				getSameIndividuals().clear();
				getSameIndividuals().addAll((Collection<? extends NamedIndividual>)newValue);
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
			case OwlPackage.SAME_INDIVIDUAL__SAME_INDIVIDUALS:
				getSameIndividuals().clear();
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
			case OwlPackage.SAME_INDIVIDUAL__SAME_INDIVIDUALS:
				return sameIndividuals != null && !sameIndividuals.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //SameIndividualImpl
