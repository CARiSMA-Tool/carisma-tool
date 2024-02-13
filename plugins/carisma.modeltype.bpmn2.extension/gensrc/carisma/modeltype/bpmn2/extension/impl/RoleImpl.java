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
package carisma.modeltype.bpmn2.extension.impl;


import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.EObjectWithInverseResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;

import carisma.modeltype.bpmn2.extension.ExtensionPackage;
import carisma.modeltype.bpmn2.extension.Performer;
import carisma.modeltype.bpmn2.extension.Role;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Role</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.RoleImpl#getName <em>Name</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.RoleImpl#getMember <em>Member</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.RoleImpl#getSuper <em>Super</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.RoleImpl#getSub <em>Sub</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.RoleImpl#getConflict <em>Conflict</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RoleImpl extends BaseElementImpl implements Role {
	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * The cached value of the '{@link #getMember() <em>Member</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getMember()
	 * @generated
	 * @ordered
	 */
	protected EList<Performer> member;

	/**
	 * The cached value of the '{@link #getSuper() <em>Super</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSuper()
	 * @generated
	 * @ordered
	 */
	protected Role super_;

	/**
	 * The cached value of the '{@link #getSub() <em>Sub</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSub()
	 * @generated
	 * @ordered
	 */
	protected EList<Role> sub;

	/**
	 * The cached value of the '{@link #getConflict() <em>Conflict</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getConflict()
	 * @generated
	 * @ordered
	 */
	protected EList<Role> conflict;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected RoleImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExtensionPackage.Literals.ROLE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getName() {
		return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setName(String newName) {
		String oldName = name;
		name = newName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExtensionPackage.ROLE__NAME, oldName, name));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Performer> getMember() {
		if (member == null) {
			member = new EObjectWithInverseResolvingEList.ManyInverse<Performer>(Performer.class, this, ExtensionPackage.ROLE__MEMBER, ExtensionPackage.PERFORMER__ROLE);
		}
		return member;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Role getSuper() {
		if (super_ != null && super_.eIsProxy()) {
			InternalEObject oldSuper = (InternalEObject)super_;
			super_ = (Role)eResolveProxy(oldSuper);
			if (super_ != oldSuper) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, ExtensionPackage.ROLE__SUPER, oldSuper, super_));
			}
		}
		return super_;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Role basicGetSuper() {
		return super_;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetSuper(Role newSuper, NotificationChain msgs) {
		Role oldSuper = super_;
		super_ = newSuper;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ExtensionPackage.ROLE__SUPER, oldSuper, newSuper);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSuper(Role newSuper) {
		if (newSuper != super_) {
			NotificationChain msgs = null;
			if (super_ != null)
				msgs = ((InternalEObject)super_).eInverseRemove(this, ExtensionPackage.ROLE__SUB, Role.class, msgs);
			if (newSuper != null)
				msgs = ((InternalEObject)newSuper).eInverseAdd(this, ExtensionPackage.ROLE__SUB, Role.class, msgs);
			msgs = basicSetSuper(newSuper, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExtensionPackage.ROLE__SUPER, newSuper, newSuper));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Role> getSub() {
		if (sub == null) {
			sub = new EObjectWithInverseResolvingEList<Role>(Role.class, this, ExtensionPackage.ROLE__SUB, ExtensionPackage.ROLE__SUPER);
		}
		return sub;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Role> getConflict() {
		if (conflict == null) {
			conflict = new EObjectResolvingEList<Role>(Role.class, this, ExtensionPackage.ROLE__CONFLICT);
		}
		return conflict;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExtensionPackage.ROLE__MEMBER:
				return ((InternalEList<InternalEObject>)(InternalEList<?>)getMember()).basicAdd(otherEnd, msgs);
			case ExtensionPackage.ROLE__SUPER:
				if (super_ != null)
					msgs = ((InternalEObject)super_).eInverseRemove(this, ExtensionPackage.ROLE__SUB, Role.class, msgs);
				return basicSetSuper((Role)otherEnd, msgs);
			case ExtensionPackage.ROLE__SUB:
				return ((InternalEList<InternalEObject>)(InternalEList<?>)getSub()).basicAdd(otherEnd, msgs);
		}
		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExtensionPackage.ROLE__MEMBER:
				return ((InternalEList<?>)getMember()).basicRemove(otherEnd, msgs);
			case ExtensionPackage.ROLE__SUPER:
				return basicSetSuper(null, msgs);
			case ExtensionPackage.ROLE__SUB:
				return ((InternalEList<?>)getSub()).basicRemove(otherEnd, msgs);
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
			case ExtensionPackage.ROLE__NAME:
				return getName();
			case ExtensionPackage.ROLE__MEMBER:
				return getMember();
			case ExtensionPackage.ROLE__SUPER:
				if (resolve) return getSuper();
				return basicGetSuper();
			case ExtensionPackage.ROLE__SUB:
				return getSub();
			case ExtensionPackage.ROLE__CONFLICT:
				return getConflict();
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
			case ExtensionPackage.ROLE__NAME:
				setName((String)newValue);
				return;
			case ExtensionPackage.ROLE__MEMBER:
				getMember().clear();
				getMember().addAll((Collection<? extends Performer>)newValue);
				return;
			case ExtensionPackage.ROLE__SUPER:
				setSuper((Role)newValue);
				return;
			case ExtensionPackage.ROLE__SUB:
				getSub().clear();
				getSub().addAll((Collection<? extends Role>)newValue);
				return;
			case ExtensionPackage.ROLE__CONFLICT:
				getConflict().clear();
				getConflict().addAll((Collection<? extends Role>)newValue);
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
			case ExtensionPackage.ROLE__NAME:
				setName(NAME_EDEFAULT);
				return;
			case ExtensionPackage.ROLE__MEMBER:
				getMember().clear();
				return;
			case ExtensionPackage.ROLE__SUPER:
				setSuper((Role)null);
				return;
			case ExtensionPackage.ROLE__SUB:
				getSub().clear();
				return;
			case ExtensionPackage.ROLE__CONFLICT:
				getConflict().clear();
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
			case ExtensionPackage.ROLE__NAME:
				return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
			case ExtensionPackage.ROLE__MEMBER:
				return member != null && !member.isEmpty();
			case ExtensionPackage.ROLE__SUPER:
				return super_ != null;
			case ExtensionPackage.ROLE__SUB:
				return sub != null && !sub.isEmpty();
			case ExtensionPackage.ROLE__CONFLICT:
				return conflict != null && !conflict.isEmpty();
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
		result.append(" (name: ");
		result.append(name);
		result.append(')');
		return result.toString();
	}

} //RoleImpl
