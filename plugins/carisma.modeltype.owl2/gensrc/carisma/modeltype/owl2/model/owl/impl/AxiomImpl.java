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
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import carisma.modeltype.owl2.model.owl.Annotation;
import carisma.modeltype.owl2.model.owl.Axiom;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Axiom</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.AxiomImpl#getAxiomAnnotations <em>Axiom Annotations</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.AxiomImpl#getAxiomId <em>Axiom Id</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public abstract class AxiomImpl extends EObjectImpl implements Axiom {
	/**
	 * The cached value of the '{@link #getAxiomAnnotations() <em>Axiom Annotations</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAxiomAnnotations()
	 * @generated
	 * @ordered
	 */
	protected EList<Annotation> axiomAnnotations;

	/**
	 * The default value of the '{@link #getAxiomId() <em>Axiom Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAxiomId()
	 * @generated
	 * @ordered
	 */
	protected static final String AXIOM_ID_EDEFAULT = null;
	/**
	 * The cached value of the '{@link #getAxiomId() <em>Axiom Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAxiomId()
	 * @generated
	 * @ordered
	 */
	protected String axiomId = AXIOM_ID_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected AxiomImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.AXIOM;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Annotation> getAxiomAnnotations() {
		if (axiomAnnotations == null) {
			axiomAnnotations = new EObjectResolvingEList<Annotation>(Annotation.class, this, OwlPackage.AXIOM__AXIOM_ANNOTATIONS);
		}
		return axiomAnnotations;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getAxiomId() {
		return axiomId;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAxiomId(String newAxiomId) {
		String oldAxiomId = axiomId;
		axiomId = newAxiomId;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.AXIOM__AXIOM_ID, oldAxiomId, axiomId));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.AXIOM__AXIOM_ANNOTATIONS:
				return getAxiomAnnotations();
			case OwlPackage.AXIOM__AXIOM_ID:
				return getAxiomId();
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
			case OwlPackage.AXIOM__AXIOM_ANNOTATIONS:
				getAxiomAnnotations().clear();
				getAxiomAnnotations().addAll((Collection<? extends Annotation>)newValue);
				return;
			case OwlPackage.AXIOM__AXIOM_ID:
				setAxiomId((String)newValue);
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
			case OwlPackage.AXIOM__AXIOM_ANNOTATIONS:
				getAxiomAnnotations().clear();
				return;
			case OwlPackage.AXIOM__AXIOM_ID:
				setAxiomId(AXIOM_ID_EDEFAULT);
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
			case OwlPackage.AXIOM__AXIOM_ANNOTATIONS:
				return axiomAnnotations != null && !axiomAnnotations.isEmpty();
			case OwlPackage.AXIOM__AXIOM_ID:
				return AXIOM_ID_EDEFAULT == null ? axiomId != null : !AXIOM_ID_EDEFAULT.equals(axiomId);
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (axiomId: ");
		result.append(axiomId);
		result.append(')');
		return result.toString();
	}

} //AxiomImpl
