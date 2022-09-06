/**
 */
package carisma.profile.umlsec.umlsec4ids.impl;

import carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage;
import carisma.profile.umlsec.umlsec4ids.dataprovenancetracking;

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
 * An implementation of the model object '<em><b>dataprovenancetracking</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.dataprovenancetrackingImpl#getBase_Package <em>Base Package</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.dataprovenancetrackingImpl#getStart_action <em>Start action</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.dataprovenancetrackingImpl#getStop_action <em>Stop action</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.dataprovenancetrackingImpl#getProtected <em>Protected</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.dataprovenancetrackingImpl#getRight <em>Right</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.dataprovenancetrackingImpl#getClearing_house <em>Clearing house</em>}</li>
 * </ul>
 *
 * @generated
 */
public class dataprovenancetrackingImpl extends MinimalEObjectImpl.Container implements dataprovenancetracking {
	/**
	 * The cached value of the '{@link #getBase_Package() <em>Base Package</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getBase_Package()
	 * @generated
	 * @ordered
	 */
	protected org.eclipse.uml2.uml.Package base_Package;

	/**
	 * The cached value of the '{@link #getStart_action() <em>Start action</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStart_action()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> start_action;

	/**
	 * The cached value of the '{@link #getStop_action() <em>Stop action</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getStop_action()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> stop_action;

	/**
	 * The cached value of the '{@link #getProtected() <em>Protected</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProtected()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> protected_;

	/**
	 * The cached value of the '{@link #getRight() <em>Right</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRight()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> right;

	/**
	 * The cached value of the '{@link #getClearing_house() <em>Clearing house</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getClearing_house()
	 * @generated
	 * @ordered
	 */
	protected EList<ActivityPartition> clearing_house;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected dataprovenancetrackingImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Umlsec4idsPackage.Literals.DATAPROVENANCETRACKING;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public org.eclipse.uml2.uml.Package getBase_Package() {
		if (base_Package != null && base_Package.eIsProxy()) {
			InternalEObject oldBase_Package = (InternalEObject)base_Package;
			base_Package = (org.eclipse.uml2.uml.Package)eResolveProxy(oldBase_Package);
			if (base_Package != oldBase_Package) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Umlsec4idsPackage.DATAPROVENANCETRACKING__BASE_PACKAGE, oldBase_Package, base_Package));
			}
		}
		return base_Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public org.eclipse.uml2.uml.Package basicGetBase_Package() {
		return base_Package;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void setBase_Package(org.eclipse.uml2.uml.Package newBase_Package) {
		org.eclipse.uml2.uml.Package oldBase_Package = base_Package;
		base_Package = newBase_Package;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, Umlsec4idsPackage.DATAPROVENANCETRACKING__BASE_PACKAGE, oldBase_Package, base_Package));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getStart_action() {
		if (start_action == null) {
			start_action = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.DATAPROVENANCETRACKING__START_ACTION);
		}
		return start_action;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getStop_action() {
		if (stop_action == null) {
			stop_action = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.DATAPROVENANCETRACKING__STOP_ACTION);
		}
		return stop_action;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getProtected() {
		if (protected_ == null) {
			protected_ = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.DATAPROVENANCETRACKING__PROTECTED);
		}
		return protected_;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getRight() {
		if (right == null) {
			right = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.DATAPROVENANCETRACKING__RIGHT);
		}
		return right;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<ActivityPartition> getClearing_house() {
		if (clearing_house == null) {
			clearing_house = new EObjectResolvingEList<ActivityPartition>(ActivityPartition.class, this, Umlsec4idsPackage.DATAPROVENANCETRACKING__CLEARING_HOUSE);
		}
		return clearing_house;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__BASE_PACKAGE:
				if (resolve) return getBase_Package();
				return basicGetBase_Package();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__START_ACTION:
				return getStart_action();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__STOP_ACTION:
				return getStop_action();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__PROTECTED:
				return getProtected();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__RIGHT:
				return getRight();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__CLEARING_HOUSE:
				return getClearing_house();
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
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)newValue);
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__START_ACTION:
				getStart_action().clear();
				getStart_action().addAll((Collection<? extends Action>)newValue);
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__STOP_ACTION:
				getStop_action().clear();
				getStop_action().addAll((Collection<? extends Action>)newValue);
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__PROTECTED:
				getProtected().clear();
				getProtected().addAll((Collection<? extends Action>)newValue);
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__RIGHT:
				getRight().clear();
				getRight().addAll((Collection<? extends Action>)newValue);
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__CLEARING_HOUSE:
				getClearing_house().clear();
				getClearing_house().addAll((Collection<? extends ActivityPartition>)newValue);
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
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__BASE_PACKAGE:
				setBase_Package((org.eclipse.uml2.uml.Package)null);
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__START_ACTION:
				getStart_action().clear();
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__STOP_ACTION:
				getStop_action().clear();
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__PROTECTED:
				getProtected().clear();
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__RIGHT:
				getRight().clear();
				return;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__CLEARING_HOUSE:
				getClearing_house().clear();
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
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__BASE_PACKAGE:
				return base_Package != null;
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__START_ACTION:
				return start_action != null && !start_action.isEmpty();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__STOP_ACTION:
				return stop_action != null && !stop_action.isEmpty();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__PROTECTED:
				return protected_ != null && !protected_.isEmpty();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__RIGHT:
				return right != null && !right.isEmpty();
			case Umlsec4idsPackage.DATAPROVENANCETRACKING__CLEARING_HOUSE:
				return clearing_house != null && !clearing_house.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //dataprovenancetrackingImpl
