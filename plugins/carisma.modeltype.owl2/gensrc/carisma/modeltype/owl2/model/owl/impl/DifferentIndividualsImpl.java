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

import carisma.modeltype.owl2.model.owl.DifferentIndividuals;
import carisma.modeltype.owl2.model.owl.NamedIndividual;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Different Individuals</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.DifferentIndividualsImpl#getDifferentIndividuals <em>Different Individuals</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class DifferentIndividualsImpl extends AssertionImpl implements DifferentIndividuals {
	/**
	 * The cached value of the '{@link #getDifferentIndividuals() <em>Different Individuals</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDifferentIndividuals()
	 * @generated
	 * @ordered
	 */
	protected EList<NamedIndividual> differentIndividuals;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected DifferentIndividualsImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.DIFFERENT_INDIVIDUALS;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<NamedIndividual> getDifferentIndividuals() {
		if (differentIndividuals == null) {
			differentIndividuals = new EObjectResolvingEList<NamedIndividual>(NamedIndividual.class, this, OwlPackage.DIFFERENT_INDIVIDUALS__DIFFERENT_INDIVIDUALS);
		}
		return differentIndividuals;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.DIFFERENT_INDIVIDUALS__DIFFERENT_INDIVIDUALS:
				return getDifferentIndividuals();
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
			case OwlPackage.DIFFERENT_INDIVIDUALS__DIFFERENT_INDIVIDUALS:
				getDifferentIndividuals().clear();
				getDifferentIndividuals().addAll((Collection<? extends NamedIndividual>)newValue);
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
			case OwlPackage.DIFFERENT_INDIVIDUALS__DIFFERENT_INDIVIDUALS:
				getDifferentIndividuals().clear();
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
			case OwlPackage.DIFFERENT_INDIVIDUALS__DIFFERENT_INDIVIDUALS:
				return differentIndividuals != null && !differentIndividuals.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //DifferentIndividualsImpl
