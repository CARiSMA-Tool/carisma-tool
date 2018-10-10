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

import carisma.modeltype.owl2.model.owl.ObjectPropertyExpression;
import carisma.modeltype.owl2.model.owl.OwlPackage;
import carisma.modeltype.owl2.model.owl.SubObjectPropertyOf;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Sub Object Property Of</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyOfImpl#getSuperObjectPropertyExpression <em>Super Object Property Expression</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.SubObjectPropertyOfImpl#getSubObjectPropertyExpressions <em>Sub Object Property Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SubObjectPropertyOfImpl extends ObjectPropertyAxiomImpl implements SubObjectPropertyOf {
	/**
	 * The cached value of the '{@link #getSuperObjectPropertyExpression() <em>Super Object Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSuperObjectPropertyExpression()
	 * @generated
	 * @ordered
	 */
	protected ObjectPropertyExpression superObjectPropertyExpression;

	/**
	 * The cached value of the '{@link #getSubObjectPropertyExpressions() <em>Sub Object Property Expressions</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSubObjectPropertyExpressions()
	 * @generated
	 * @ordered
	 */
	protected ObjectPropertyExpression subObjectPropertyExpressions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SubObjectPropertyOfImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.SUB_OBJECT_PROPERTY_OF;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyExpression getSuperObjectPropertyExpression() {
		if (superObjectPropertyExpression != null && superObjectPropertyExpression.eIsProxy()) {
			InternalEObject oldSuperObjectPropertyExpression = (InternalEObject)superObjectPropertyExpression;
			superObjectPropertyExpression = (ObjectPropertyExpression)eResolveProxy(oldSuperObjectPropertyExpression);
			if (superObjectPropertyExpression != oldSuperObjectPropertyExpression) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.SUB_OBJECT_PROPERTY_OF__SUPER_OBJECT_PROPERTY_EXPRESSION, oldSuperObjectPropertyExpression, superObjectPropertyExpression));
			}
		}
		return superObjectPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyExpression basicGetSuperObjectPropertyExpression() {
		return superObjectPropertyExpression;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSuperObjectPropertyExpression(ObjectPropertyExpression newSuperObjectPropertyExpression) {
		ObjectPropertyExpression oldSuperObjectPropertyExpression = superObjectPropertyExpression;
		superObjectPropertyExpression = newSuperObjectPropertyExpression;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.SUB_OBJECT_PROPERTY_OF__SUPER_OBJECT_PROPERTY_EXPRESSION, oldSuperObjectPropertyExpression, superObjectPropertyExpression));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyExpression getSubObjectPropertyExpressions() {
		if (subObjectPropertyExpressions != null && subObjectPropertyExpressions.eIsProxy()) {
			InternalEObject oldSubObjectPropertyExpressions = (InternalEObject)subObjectPropertyExpressions;
			subObjectPropertyExpressions = (ObjectPropertyExpression)eResolveProxy(oldSubObjectPropertyExpressions);
			if (subObjectPropertyExpressions != oldSubObjectPropertyExpressions) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.SUB_OBJECT_PROPERTY_OF__SUB_OBJECT_PROPERTY_EXPRESSIONS, oldSubObjectPropertyExpressions, subObjectPropertyExpressions));
			}
		}
		return subObjectPropertyExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ObjectPropertyExpression basicGetSubObjectPropertyExpressions() {
		return subObjectPropertyExpressions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSubObjectPropertyExpressions(ObjectPropertyExpression newSubObjectPropertyExpressions) {
		ObjectPropertyExpression oldSubObjectPropertyExpressions = subObjectPropertyExpressions;
		subObjectPropertyExpressions = newSubObjectPropertyExpressions;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.SUB_OBJECT_PROPERTY_OF__SUB_OBJECT_PROPERTY_EXPRESSIONS, oldSubObjectPropertyExpressions, subObjectPropertyExpressions));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.SUB_OBJECT_PROPERTY_OF__SUPER_OBJECT_PROPERTY_EXPRESSION:
				if (resolve) return getSuperObjectPropertyExpression();
				return basicGetSuperObjectPropertyExpression();
			case OwlPackage.SUB_OBJECT_PROPERTY_OF__SUB_OBJECT_PROPERTY_EXPRESSIONS:
				if (resolve) return getSubObjectPropertyExpressions();
				return basicGetSubObjectPropertyExpressions();
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
			case OwlPackage.SUB_OBJECT_PROPERTY_OF__SUPER_OBJECT_PROPERTY_EXPRESSION:
				setSuperObjectPropertyExpression((ObjectPropertyExpression)newValue);
				return;
			case OwlPackage.SUB_OBJECT_PROPERTY_OF__SUB_OBJECT_PROPERTY_EXPRESSIONS:
				setSubObjectPropertyExpressions((ObjectPropertyExpression)newValue);
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
			case OwlPackage.SUB_OBJECT_PROPERTY_OF__SUPER_OBJECT_PROPERTY_EXPRESSION:
				setSuperObjectPropertyExpression((ObjectPropertyExpression)null);
				return;
			case OwlPackage.SUB_OBJECT_PROPERTY_OF__SUB_OBJECT_PROPERTY_EXPRESSIONS:
				setSubObjectPropertyExpressions((ObjectPropertyExpression)null);
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
			case OwlPackage.SUB_OBJECT_PROPERTY_OF__SUPER_OBJECT_PROPERTY_EXPRESSION:
				return superObjectPropertyExpression != null;
			case OwlPackage.SUB_OBJECT_PROPERTY_OF__SUB_OBJECT_PROPERTY_EXPRESSIONS:
				return subObjectPropertyExpressions != null;
		}
		return super.eIsSet(featureID);
	}

} //SubObjectPropertyOfImpl
