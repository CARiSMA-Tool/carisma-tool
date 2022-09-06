/**
 */
package carisma.profile.umlsec.umlsec4ids.impl;

import carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage;
import carisma.profile.umlsec.umlsec4ids.datausagecontrol;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityPartition;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>datausagecontrol</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.datausagecontrolImpl#getBase_ActivityPartition <em>Base Activity Partition</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.datausagecontrolImpl#getPermission <em>Permission</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.datausagecontrolImpl#getObligation_start <em>Obligation start</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.datausagecontrolImpl#getObligation_stop <em>Obligation stop</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.datausagecontrolImpl#getProhibition <em>Prohibition</em>}</li>
 * </ul>
 *
 * @generated
 */
public class datausagecontrolImpl extends MinimalEObjectImpl.Container implements datausagecontrol {
	/**
	 * The cached value of the '{@link #getBase_ActivityPartition() <em>Base Activity Partition</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_ActivityPartition()
	 * @generated
	 * @ordered
	 */
	protected ActivityPartition base_ActivityPartition;

	/**
	 * The cached value of the '{@link #getPermission() <em>Permission</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getPermission()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> permission;

	/**
	 * The cached value of the '{@link #getObligation_start() <em>Obligation start</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getObligation_start()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> obligation_start;

	/**
	 * The cached value of the '{@link #getObligation_stop() <em>Obligation stop</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getObligation_stop()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> obligation_stop;

	/**
	 * The cached value of the '{@link #getProhibition() <em>Prohibition</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProhibition()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> prohibition;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected datausagecontrolImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Umlsec4idsPackage.Literals.DATAUSAGECONTROL;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public ActivityPartition getBase_ActivityPartition() {
		if (base_ActivityPartition != null && base_ActivityPartition.eIsProxy()) {
			InternalEObject oldBase_ActivityPartition = (InternalEObject)base_ActivityPartition;
			base_ActivityPartition = (ActivityPartition)eResolveProxy(oldBase_ActivityPartition);
			if (base_ActivityPartition != oldBase_ActivityPartition) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Umlsec4idsPackage.DATAUSAGECONTROL__BASE_ACTIVITY_PARTITION, oldBase_ActivityPartition, base_ActivityPartition));
			}
		}
		return base_ActivityPartition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public ActivityPartition basicGetBase_ActivityPartition() {
		return base_ActivityPartition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_ActivityPartition(ActivityPartition newBase_ActivityPartition) {
		ActivityPartition oldBase_ActivityPartition = base_ActivityPartition;
		base_ActivityPartition = newBase_ActivityPartition;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Umlsec4idsPackage.DATAUSAGECONTROL__BASE_ACTIVITY_PARTITION, oldBase_ActivityPartition, base_ActivityPartition));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getPermission() {
		if (permission == null) {
			permission = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.DATAUSAGECONTROL__PERMISSION);
		}
		return permission;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getObligation_start() {
		if (obligation_start == null) {
			obligation_start = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_START);
		}
		return obligation_start;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getObligation_stop() {
		if (obligation_stop == null) {
			obligation_stop = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_STOP);
		}
		return obligation_stop;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getProhibition() {
		if (prohibition == null) {
			prohibition = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.DATAUSAGECONTROL__PROHIBITION);
		}
		return prohibition;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Umlsec4idsPackage.DATAUSAGECONTROL__BASE_ACTIVITY_PARTITION:
				if (resolve) return getBase_ActivityPartition();
				return basicGetBase_ActivityPartition();
			case Umlsec4idsPackage.DATAUSAGECONTROL__PERMISSION:
				return getPermission();
			case Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_START:
				return getObligation_start();
			case Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_STOP:
				return getObligation_stop();
			case Umlsec4idsPackage.DATAUSAGECONTROL__PROHIBITION:
				return getProhibition();
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
			case Umlsec4idsPackage.DATAUSAGECONTROL__BASE_ACTIVITY_PARTITION:
				setBase_ActivityPartition((ActivityPartition)newValue);
				return;
			case Umlsec4idsPackage.DATAUSAGECONTROL__PERMISSION:
				getPermission().clear();
				getPermission().addAll((Collection<? extends Action>)newValue);
				return;
			case Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_START:
				getObligation_start().clear();
				getObligation_start().addAll((Collection<? extends Action>)newValue);
				return;
			case Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_STOP:
				getObligation_stop().clear();
				getObligation_stop().addAll((Collection<? extends Action>)newValue);
				return;
			case Umlsec4idsPackage.DATAUSAGECONTROL__PROHIBITION:
				getProhibition().clear();
				getProhibition().addAll((Collection<? extends Action>)newValue);
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
			case Umlsec4idsPackage.DATAUSAGECONTROL__BASE_ACTIVITY_PARTITION:
				setBase_ActivityPartition((ActivityPartition)null);
				return;
			case Umlsec4idsPackage.DATAUSAGECONTROL__PERMISSION:
				getPermission().clear();
				return;
			case Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_START:
				getObligation_start().clear();
				return;
			case Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_STOP:
				getObligation_stop().clear();
				return;
			case Umlsec4idsPackage.DATAUSAGECONTROL__PROHIBITION:
				getProhibition().clear();
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
			case Umlsec4idsPackage.DATAUSAGECONTROL__BASE_ACTIVITY_PARTITION:
				return base_ActivityPartition != null;
			case Umlsec4idsPackage.DATAUSAGECONTROL__PERMISSION:
				return permission != null && !permission.isEmpty();
			case Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_START:
				return obligation_start != null && !obligation_start.isEmpty();
			case Umlsec4idsPackage.DATAUSAGECONTROL__OBLIGATION_STOP:
				return obligation_stop != null && !obligation_stop.isEmpty();
			case Umlsec4idsPackage.DATAUSAGECONTROL__PROHIBITION:
				return prohibition != null && !prohibition.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //datausagecontrolImpl
