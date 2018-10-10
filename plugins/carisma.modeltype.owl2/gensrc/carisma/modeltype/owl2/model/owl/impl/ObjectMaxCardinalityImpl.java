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

import java.util.Map;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.plugin.EcorePlugin;
import org.eclipse.emf.ecore.util.EObjectValidator;

import carisma.modeltype.owl2.model.owl.ClassExpression;
import carisma.modeltype.owl2.model.owl.ObjectMaxCardinality;
import carisma.modeltype.owl2.model.owl.ObjectPropertyExpression;
import carisma.modeltype.owl2.model.owl.OwlPackage;
import carisma.modeltype.owl2.model.owl.util.OwlValidator;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Object Max Cardinality</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.ObjectMaxCardinalityImpl#getCardinality <em>Cardinality</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.ObjectMaxCardinalityImpl#getClassExpression <em>Class Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.ObjectMaxCardinalityImpl#getObjectPropertyExpression <em>Object Property Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ObjectMaxCardinalityImpl extends ClassExpressionImpl implements ObjectMaxCardinality {
	/**
	 * The default value of the '{@link #getCardinality() <em>Cardinality</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCardinality()
	 * @generated
	 * @ordered
	 */
	protected static final int CARDINALITY_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getCardinality() <em>Cardinality</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCardinality()
	 * @generated
	 * @ordered
	 */
	protected int cardinality = CARDINALITY_EDEFAULT;

	/**
	 * The cached value of the '{@link #getClassExpression() <em>Class Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getClassExpression()
	 * @generated
	 * @ordered
	 */
	protected ClassExpression classExpression;

	/**
	 * The cached value of the '{@link #getObjectPropertyExpression() <em>Object Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getObjectPropertyExpression()
	 * @generated
	 * @ordered
	 */
	protected ObjectPropertyExpression objectPropertyExpression;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ObjectMaxCardinalityImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.OBJECT_MAX_CARDINALITY;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public int getCardinality() {
		return cardinality;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setCardinality(int newCardinality) {
		int oldCardinality = cardinality;
		cardinality = newCardinality;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.OBJECT_MAX_CARDINALITY__CARDINALITY, oldCardinality, cardinality));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassExpression getClassExpression() {
		if (classExpression != null && classExpression.eIsProxy()) {
			InternalEObject oldClassExpression = (InternalEObject)classExpression;
			classExpression = (ClassExpression)eResolveProxy(oldClassExpression);
			if (classExpression != oldClassExpression) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.OBJECT_MAX_CARDINALITY__CLASS_EXPRESSION, oldClassExpression, classExpression));
			}
		}
		return classExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ClassExpression basicGetClassExpression() {
		return classExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setClassExpression(ClassExpression newClassExpression) {
		ClassExpression oldClassExpression = classExpression;
		classExpression = newClassExpression;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.OBJECT_MAX_CARDINALITY__CLASS_EXPRESSION, oldClassExpression, classExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyExpression getObjectPropertyExpression() {
		if (objectPropertyExpression != null && objectPropertyExpression.eIsProxy()) {
			InternalEObject oldObjectPropertyExpression = (InternalEObject)objectPropertyExpression;
			objectPropertyExpression = (ObjectPropertyExpression)eResolveProxy(oldObjectPropertyExpression);
			if (objectPropertyExpression != oldObjectPropertyExpression) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.OBJECT_MAX_CARDINALITY__OBJECT_PROPERTY_EXPRESSION, oldObjectPropertyExpression, objectPropertyExpression));
			}
		}
		return objectPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyExpression basicGetObjectPropertyExpression() {
		return objectPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setObjectPropertyExpression(ObjectPropertyExpression newObjectPropertyExpression) {
		ObjectPropertyExpression oldObjectPropertyExpression = objectPropertyExpression;
		objectPropertyExpression = newObjectPropertyExpression;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.OBJECT_MAX_CARDINALITY__OBJECT_PROPERTY_EXPRESSION, oldObjectPropertyExpression, objectPropertyExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean Thecardinalitymustbenonnegative(DiagnosticChain diagnostics, Map context) {
		// TODO: implement this method
		// -> specify the condition that violates the invariant
		// -> verify the details of the diagnostic, including severity and message
		// Ensure that you remove @generated or mark it @generated NOT
		if (false) {
			if (diagnostics != null) {
				diagnostics.add
					(new BasicDiagnostic
						(Diagnostic.ERROR,
						 OwlValidator.DIAGNOSTIC_SOURCE,
						 OwlValidator.OBJECT_MAX_CARDINALITY__THECARDINALITYMUSTBENONNEGATIVE,
						 EcorePlugin.INSTANCE.getString("_UI_GenericInvariant_diagnostic", new Object[] { "Thecardinalitymustbenonnegative", EObjectValidator.getObjectLabel(this, context) }),
						 new Object [] { this }));
			}
			return false;
		}
		return true;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.OBJECT_MAX_CARDINALITY__CARDINALITY:
				return getCardinality();
			case OwlPackage.OBJECT_MAX_CARDINALITY__CLASS_EXPRESSION:
				if (resolve) return getClassExpression();
				return basicGetClassExpression();
			case OwlPackage.OBJECT_MAX_CARDINALITY__OBJECT_PROPERTY_EXPRESSION:
				if (resolve) return getObjectPropertyExpression();
				return basicGetObjectPropertyExpression();
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
			case OwlPackage.OBJECT_MAX_CARDINALITY__CARDINALITY:
				setCardinality((Integer)newValue);
				return;
			case OwlPackage.OBJECT_MAX_CARDINALITY__CLASS_EXPRESSION:
				setClassExpression((ClassExpression)newValue);
				return;
			case OwlPackage.OBJECT_MAX_CARDINALITY__OBJECT_PROPERTY_EXPRESSION:
				setObjectPropertyExpression((ObjectPropertyExpression)newValue);
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
			case OwlPackage.OBJECT_MAX_CARDINALITY__CARDINALITY:
				setCardinality(CARDINALITY_EDEFAULT);
				return;
			case OwlPackage.OBJECT_MAX_CARDINALITY__CLASS_EXPRESSION:
				setClassExpression((ClassExpression)null);
				return;
			case OwlPackage.OBJECT_MAX_CARDINALITY__OBJECT_PROPERTY_EXPRESSION:
				setObjectPropertyExpression((ObjectPropertyExpression)null);
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
			case OwlPackage.OBJECT_MAX_CARDINALITY__CARDINALITY:
				return cardinality != CARDINALITY_EDEFAULT;
			case OwlPackage.OBJECT_MAX_CARDINALITY__CLASS_EXPRESSION:
				return classExpression != null;
			case OwlPackage.OBJECT_MAX_CARDINALITY__OBJECT_PROPERTY_EXPRESSION:
				return objectPropertyExpression != null;
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
		result.append(" (cardinality: ");
		result.append(cardinality);
		result.append(')');
		return result.toString();
	}

} //ObjectMaxCardinalityImpl
