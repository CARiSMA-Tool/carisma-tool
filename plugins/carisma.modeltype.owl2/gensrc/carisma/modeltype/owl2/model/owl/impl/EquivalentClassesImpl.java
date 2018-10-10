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
import carisma.modeltype.owl2.model.owl.EquivalentClasses;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Equivalent Classes</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.EquivalentClassesImpl#getEquivalentClassExpressions <em>Equivalent Class Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class EquivalentClassesImpl extends ClassAxiomImpl implements EquivalentClasses {
	/**
	 * The cached value of the '{@link #getEquivalentClassExpressions() <em>Equivalent Class Expressions</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getEquivalentClassExpressions()
	 * @generated
	 * @ordered
	 */
	protected EList<ClassExpression> equivalentClassExpressions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EquivalentClassesImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.EQUIVALENT_CLASSES;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ClassExpression> getEquivalentClassExpressions() {
		if (equivalentClassExpressions == null) {
			equivalentClassExpressions = new EObjectResolvingEList<ClassExpression>(ClassExpression.class, this, OwlPackage.EQUIVALENT_CLASSES__EQUIVALENT_CLASS_EXPRESSIONS);
		}
		return equivalentClassExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.EQUIVALENT_CLASSES__EQUIVALENT_CLASS_EXPRESSIONS:
				return getEquivalentClassExpressions();
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
			case OwlPackage.EQUIVALENT_CLASSES__EQUIVALENT_CLASS_EXPRESSIONS:
				getEquivalentClassExpressions().clear();
				getEquivalentClassExpressions().addAll((Collection<? extends ClassExpression>)newValue);
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
			case OwlPackage.EQUIVALENT_CLASSES__EQUIVALENT_CLASS_EXPRESSIONS:
				getEquivalentClassExpressions().clear();
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
			case OwlPackage.EQUIVALENT_CLASSES__EQUIVALENT_CLASS_EXPRESSIONS:
				return equivalentClassExpressions != null && !equivalentClassExpressions.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //EquivalentClassesImpl
