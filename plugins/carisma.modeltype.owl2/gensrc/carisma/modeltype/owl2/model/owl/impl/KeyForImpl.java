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

import carisma.modeltype.owl2.model.owl.ClassExpression;
import carisma.modeltype.owl2.model.owl.DataPropertyExpression;
import carisma.modeltype.owl2.model.owl.KeyFor;
import carisma.modeltype.owl2.model.owl.ObjectPropertyExpression;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Key For</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.KeyForImpl#getClassExpression <em>Class Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.KeyForImpl#getDataPropertyExpressions <em>Data Property Expressions</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.KeyForImpl#getObjectPropertyExpressions <em>Object Property Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class KeyForImpl extends ObjectAndDataPropertyAxiomImpl implements KeyFor {
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
	 * The cached value of the '{@link #getDataPropertyExpressions() <em>Data Property Expressions</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getDataPropertyExpressions()
	 * @generated
	 * @ordered
	 */
	protected EList<DataPropertyExpression> dataPropertyExpressions;

	/**
	 * The cached value of the '{@link #getObjectPropertyExpressions() <em>Object Property Expressions</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getObjectPropertyExpressions()
	 * @generated
	 * @ordered
	 */
	protected EList<ObjectPropertyExpression> objectPropertyExpressions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected KeyForImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.KEY_FOR;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.KEY_FOR__CLASS_EXPRESSION, oldClassExpression, classExpression));
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
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.KEY_FOR__CLASS_EXPRESSION, oldClassExpression, classExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<DataPropertyExpression> getDataPropertyExpressions() {
		if (dataPropertyExpressions == null) {
			dataPropertyExpressions = new EObjectResolvingEList<DataPropertyExpression>(DataPropertyExpression.class, this, OwlPackage.KEY_FOR__DATA_PROPERTY_EXPRESSIONS);
		}
		return dataPropertyExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<ObjectPropertyExpression> getObjectPropertyExpressions() {
		if (objectPropertyExpressions == null) {
			objectPropertyExpressions = new EObjectResolvingEList<ObjectPropertyExpression>(ObjectPropertyExpression.class, this, OwlPackage.KEY_FOR__OBJECT_PROPERTY_EXPRESSIONS);
		}
		return objectPropertyExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.KEY_FOR__CLASS_EXPRESSION:
				if (resolve) return getClassExpression();
				return basicGetClassExpression();
			case OwlPackage.KEY_FOR__DATA_PROPERTY_EXPRESSIONS:
				return getDataPropertyExpressions();
			case OwlPackage.KEY_FOR__OBJECT_PROPERTY_EXPRESSIONS:
				return getObjectPropertyExpressions();
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
			case OwlPackage.KEY_FOR__CLASS_EXPRESSION:
				setClassExpression((ClassExpression)newValue);
				return;
			case OwlPackage.KEY_FOR__DATA_PROPERTY_EXPRESSIONS:
				getDataPropertyExpressions().clear();
				getDataPropertyExpressions().addAll((Collection<? extends DataPropertyExpression>)newValue);
				return;
			case OwlPackage.KEY_FOR__OBJECT_PROPERTY_EXPRESSIONS:
				getObjectPropertyExpressions().clear();
				getObjectPropertyExpressions().addAll((Collection<? extends ObjectPropertyExpression>)newValue);
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
			case OwlPackage.KEY_FOR__CLASS_EXPRESSION:
				setClassExpression((ClassExpression)null);
				return;
			case OwlPackage.KEY_FOR__DATA_PROPERTY_EXPRESSIONS:
				getDataPropertyExpressions().clear();
				return;
			case OwlPackage.KEY_FOR__OBJECT_PROPERTY_EXPRESSIONS:
				getObjectPropertyExpressions().clear();
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
			case OwlPackage.KEY_FOR__CLASS_EXPRESSION:
				return classExpression != null;
			case OwlPackage.KEY_FOR__DATA_PROPERTY_EXPRESSIONS:
				return dataPropertyExpressions != null && !dataPropertyExpressions.isEmpty();
			case OwlPackage.KEY_FOR__OBJECT_PROPERTY_EXPRESSIONS:
				return objectPropertyExpressions != null && !objectPropertyExpressions.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //KeyForImpl
