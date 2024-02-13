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
import org.eclipse.emf.ecore.impl.EObjectImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import carisma.modeltype.bpmn2.extension.ExtensionPackage;
import carisma.modeltype.bpmn2.extension.ExtensionRoot;
import carisma.modeltype.bpmn2.extension.Lane;
import carisma.modeltype.bpmn2.extension.Performer;
import carisma.modeltype.bpmn2.extension.Role;
import carisma.modeltype.bpmn2.extension.Selection;
import carisma.modeltype.bpmn2.extension.Task;
import carisma.modeltype.bpmn2.extension.WorkItem;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Root</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl#getTask <em>Task</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl#getWorkItem <em>Work Item</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl#getPerformer <em>Performer</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl#getRole <em>Role</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl#getLane <em>Lane</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extension.impl.ExtensionRootImpl#getSelection <em>Selection</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ExtensionRootImpl extends EObjectImpl implements ExtensionRoot {
	/**
	 * The cached value of the '{@link #getTask() <em>Task</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTask()
	 * @generated
	 * @ordered
	 */
	protected EList<Task> task;

	/**
	 * The cached value of the '{@link #getWorkItem() <em>Work Item</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getWorkItem()
	 * @generated
	 * @ordered
	 */
	protected EList<WorkItem> workItem;

	/**
	 * The cached value of the '{@link #getPerformer() <em>Performer</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPerformer()
	 * @generated
	 * @ordered
	 */
	protected EList<Performer> performer;

	/**
	 * The cached value of the '{@link #getRole() <em>Role</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRole()
	 * @generated
	 * @ordered
	 */
	protected EList<Role> role;

	/**
	 * The cached value of the '{@link #getLane() <em>Lane</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getLane()
	 * @generated
	 * @ordered
	 */
	protected EList<Lane> lane;

	/**
	 * The cached value of the '{@link #getSelection() <em>Selection</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSelection()
	 * @generated
	 * @ordered
	 */
	protected Selection selection;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ExtensionRootImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ExtensionPackage.Literals.EXTENSION_ROOT;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Task> getTask() {
		if (task == null) {
			task = new EObjectContainmentEList<Task>(Task.class, this, ExtensionPackage.EXTENSION_ROOT__TASK);
		}
		return task;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<WorkItem> getWorkItem() {
		if (workItem == null) {
			workItem = new EObjectContainmentEList<WorkItem>(WorkItem.class, this, ExtensionPackage.EXTENSION_ROOT__WORK_ITEM);
		}
		return workItem;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Performer> getPerformer() {
		if (performer == null) {
			performer = new EObjectContainmentEList<Performer>(Performer.class, this, ExtensionPackage.EXTENSION_ROOT__PERFORMER);
		}
		return performer;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Role> getRole() {
		if (role == null) {
			role = new EObjectContainmentEList<Role>(Role.class, this, ExtensionPackage.EXTENSION_ROOT__ROLE);
		}
		return role;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<Lane> getLane() {
		if (lane == null) {
			lane = new EObjectContainmentEList<Lane>(Lane.class, this, ExtensionPackage.EXTENSION_ROOT__LANE);
		}
		return lane;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Selection getSelection() {
		return selection;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetSelection(Selection newSelection, NotificationChain msgs) {
		Selection oldSelection = selection;
		selection = newSelection;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, ExtensionPackage.EXTENSION_ROOT__SELECTION, oldSelection, newSelection);
			if (msgs == null) msgs = notification; else msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSelection(Selection newSelection) {
		if (newSelection != selection) {
			NotificationChain msgs = null;
			if (selection != null)
				msgs = ((InternalEObject)selection).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - ExtensionPackage.EXTENSION_ROOT__SELECTION, null, msgs);
			if (newSelection != null)
				msgs = ((InternalEObject)newSelection).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - ExtensionPackage.EXTENSION_ROOT__SELECTION, null, msgs);
			msgs = basicSetSelection(newSelection, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ExtensionPackage.EXTENSION_ROOT__SELECTION, newSelection, newSelection));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case ExtensionPackage.EXTENSION_ROOT__TASK:
				return ((InternalEList<?>)getTask()).basicRemove(otherEnd, msgs);
			case ExtensionPackage.EXTENSION_ROOT__WORK_ITEM:
				return ((InternalEList<?>)getWorkItem()).basicRemove(otherEnd, msgs);
			case ExtensionPackage.EXTENSION_ROOT__PERFORMER:
				return ((InternalEList<?>)getPerformer()).basicRemove(otherEnd, msgs);
			case ExtensionPackage.EXTENSION_ROOT__ROLE:
				return ((InternalEList<?>)getRole()).basicRemove(otherEnd, msgs);
			case ExtensionPackage.EXTENSION_ROOT__LANE:
				return ((InternalEList<?>)getLane()).basicRemove(otherEnd, msgs);
			case ExtensionPackage.EXTENSION_ROOT__SELECTION:
				return basicSetSelection(null, msgs);
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
			case ExtensionPackage.EXTENSION_ROOT__TASK:
				return getTask();
			case ExtensionPackage.EXTENSION_ROOT__WORK_ITEM:
				return getWorkItem();
			case ExtensionPackage.EXTENSION_ROOT__PERFORMER:
				return getPerformer();
			case ExtensionPackage.EXTENSION_ROOT__ROLE:
				return getRole();
			case ExtensionPackage.EXTENSION_ROOT__LANE:
				return getLane();
			case ExtensionPackage.EXTENSION_ROOT__SELECTION:
				return getSelection();
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
			case ExtensionPackage.EXTENSION_ROOT__TASK:
				getTask().clear();
				getTask().addAll((Collection<? extends Task>)newValue);
				return;
			case ExtensionPackage.EXTENSION_ROOT__WORK_ITEM:
				getWorkItem().clear();
				getWorkItem().addAll((Collection<? extends WorkItem>)newValue);
				return;
			case ExtensionPackage.EXTENSION_ROOT__PERFORMER:
				getPerformer().clear();
				getPerformer().addAll((Collection<? extends Performer>)newValue);
				return;
			case ExtensionPackage.EXTENSION_ROOT__ROLE:
				getRole().clear();
				getRole().addAll((Collection<? extends Role>)newValue);
				return;
			case ExtensionPackage.EXTENSION_ROOT__LANE:
				getLane().clear();
				getLane().addAll((Collection<? extends Lane>)newValue);
				return;
			case ExtensionPackage.EXTENSION_ROOT__SELECTION:
				setSelection((Selection)newValue);
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
			case ExtensionPackage.EXTENSION_ROOT__TASK:
				getTask().clear();
				return;
			case ExtensionPackage.EXTENSION_ROOT__WORK_ITEM:
				getWorkItem().clear();
				return;
			case ExtensionPackage.EXTENSION_ROOT__PERFORMER:
				getPerformer().clear();
				return;
			case ExtensionPackage.EXTENSION_ROOT__ROLE:
				getRole().clear();
				return;
			case ExtensionPackage.EXTENSION_ROOT__LANE:
				getLane().clear();
				return;
			case ExtensionPackage.EXTENSION_ROOT__SELECTION:
				setSelection((Selection)null);
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
			case ExtensionPackage.EXTENSION_ROOT__TASK:
				return task != null && !task.isEmpty();
			case ExtensionPackage.EXTENSION_ROOT__WORK_ITEM:
				return workItem != null && !workItem.isEmpty();
			case ExtensionPackage.EXTENSION_ROOT__PERFORMER:
				return performer != null && !performer.isEmpty();
			case ExtensionPackage.EXTENSION_ROOT__ROLE:
				return role != null && !role.isEmpty();
			case ExtensionPackage.EXTENSION_ROOT__LANE:
				return lane != null && !lane.isEmpty();
			case ExtensionPackage.EXTENSION_ROOT__SELECTION:
				return selection != null;
		}
		return super.eIsSet(featureID);
	}

} //ExtensionRootImpl
