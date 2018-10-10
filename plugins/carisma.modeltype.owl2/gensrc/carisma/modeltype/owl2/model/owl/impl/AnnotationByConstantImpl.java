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

import carisma.modeltype.owl2.model.owl.AnnotationByConstant;
import carisma.modeltype.owl2.model.owl.Constant;
import carisma.modeltype.owl2.model.owl.OwlPackage;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Annotation By Constant</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.impl.AnnotationByConstantImpl#getAnnotationValue <em>Annotation Value</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class AnnotationByConstantImpl extends AnnotationImpl implements AnnotationByConstant {
	/**
	 * The cached value of the '{@link #getAnnotationValue() <em>Annotation Value</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getAnnotationValue()
	 * @generated
	 * @ordered
	 */
	protected Constant annotationValue;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected AnnotationByConstantImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return OwlPackage.Literals.ANNOTATION_BY_CONSTANT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Constant getAnnotationValue() {
		if (annotationValue != null && annotationValue.eIsProxy()) {
			InternalEObject oldAnnotationValue = (InternalEObject)annotationValue;
			annotationValue = (Constant)eResolveProxy(oldAnnotationValue);
			if (annotationValue != oldAnnotationValue) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, OwlPackage.ANNOTATION_BY_CONSTANT__ANNOTATION_VALUE, oldAnnotationValue, annotationValue));
			}
		}
		return annotationValue;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Constant basicGetAnnotationValue() {
		return annotationValue;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setAnnotationValue(Constant newAnnotationValue) {
		Constant oldAnnotationValue = annotationValue;
		annotationValue = newAnnotationValue;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, OwlPackage.ANNOTATION_BY_CONSTANT__ANNOTATION_VALUE, oldAnnotationValue, annotationValue));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case OwlPackage.ANNOTATION_BY_CONSTANT__ANNOTATION_VALUE:
				if (resolve) return getAnnotationValue();
				return basicGetAnnotationValue();
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
			case OwlPackage.ANNOTATION_BY_CONSTANT__ANNOTATION_VALUE:
				setAnnotationValue((Constant)newValue);
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
			case OwlPackage.ANNOTATION_BY_CONSTANT__ANNOTATION_VALUE:
				setAnnotationValue((Constant)null);
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
			case OwlPackage.ANNOTATION_BY_CONSTANT__ANNOTATION_VALUE:
				return annotationValue != null;
		}
		return super.eIsSet(featureID);
	}

} //AnnotationByConstantImpl
