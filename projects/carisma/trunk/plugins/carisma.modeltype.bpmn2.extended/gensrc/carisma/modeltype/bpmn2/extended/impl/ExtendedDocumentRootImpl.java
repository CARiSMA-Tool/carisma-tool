/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.bpmn2.extended.impl;



import org.eclipse.bpmn2.Definitions;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot;
import carisma.modeltype.bpmn2.extended.ExtendedPackage;
import carisma.modeltype.bpmn2.extension.ExtensionRoot;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Document Root</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extended.impl.ExtendedDocumentRootImpl#getExtendedDefinitions <em>Extended Definitions</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extended.impl.ExtendedDocumentRootImpl#getExtensionRoot <em>Extension Root</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ExtendedDocumentRootImpl extends EObjectImpl implements ExtendedDocumentRoot {
	/**
	 * The cached value of the '{@link #getExtendedDefinitions() <em>Extended Definitions</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExtendedDefinitions()
	 * @generated
	 * @ordered
	 */
	protected Definitions extendedDefinitions;

	/**
	 * The cached value of the '{@link #getExtensionRoot() <em>Extension Root</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getExtensionRoot()
	 * @generated
	 * @ordered
	 */
	protected ExtensionRoot extensionRoot;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ExtendedDocumentRootImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExtendedPackage.Literals.EXTENDED_DOCUMENT_ROOT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Definitions getExtendedDefinitions() {
		return extendedDefinitions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetExtendedDefinitions(Definitions newExtendedDefinitions, NotificationChain msgs) {
		Definitions oldExtendedDefinitions = extendedDefinitions;
		extendedDefinitions = newExtendedDefinitions;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS, oldExtendedDefinitions, newExtendedDefinitions);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setExtendedDefinitions(Definitions newExtendedDefinitions) {
		if (newExtendedDefinitions != extendedDefinitions) {
			NotificationChain msgs = null;
			if (extendedDefinitions != null)
				msgs = ((InternalEObject)extendedDefinitions).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS, null, msgs);
			if (newExtendedDefinitions != null)
				msgs = ((InternalEObject)newExtendedDefinitions).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS, null, msgs);
			msgs = basicSetExtendedDefinitions(newExtendedDefinitions, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS, newExtendedDefinitions, newExtendedDefinitions));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ExtensionRoot getExtensionRoot() {
		return extensionRoot;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetExtensionRoot(ExtensionRoot newExtensionRoot, NotificationChain msgs) {
		ExtensionRoot oldExtensionRoot = extensionRoot;
		extensionRoot = newExtensionRoot;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT, oldExtensionRoot, newExtensionRoot);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setExtensionRoot(ExtensionRoot newExtensionRoot) {
		if (newExtensionRoot != extensionRoot) {
			NotificationChain msgs = null;
			if (extensionRoot != null)
				msgs = ((InternalEObject)extensionRoot).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT, null, msgs);
			if (newExtensionRoot != null)
				msgs = ((InternalEObject)newExtensionRoot).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT, null, msgs);
			msgs = basicSetExtensionRoot(newExtensionRoot, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT, newExtensionRoot, newExtensionRoot));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS:
				return basicSetExtendedDefinitions(null, msgs);
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT:
				return basicSetExtensionRoot(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS:
				return getExtendedDefinitions();
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT:
				return getExtensionRoot();
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
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS:
				setExtendedDefinitions((Definitions)newValue);
				return;
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT:
				setExtensionRoot((ExtensionRoot)newValue);
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
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS:
				setExtendedDefinitions((Definitions)null);
				return;
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT:
				setExtensionRoot((ExtensionRoot)null);
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
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENDED_DEFINITIONS:
				return extendedDefinitions != null;
			case ExtendedPackage.EXTENDED_DOCUMENT_ROOT__EXTENSION_ROOT:
				return extensionRoot != null;
		}
		return super.eIsSet(featureID);
	}

} //ExtendedDocumentRootImpl
