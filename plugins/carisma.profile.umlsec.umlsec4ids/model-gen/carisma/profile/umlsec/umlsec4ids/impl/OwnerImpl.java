/**
 */
package carisma.profile.umlsec.umlsec4ids.impl;

import carisma.profile.umlsec.umlsec4ids.Owner;
import carisma.profile.umlsec.umlsec4ids.Umlsec4idsPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import org.eclipse.emf.ecore.util.EDataTypeUniqueEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;

import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityPartition;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Owner</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.OwnerImpl#getBase_ActivityPartition <em>Base Activity Partition</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.OwnerImpl#getProtected <em>Protected</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.OwnerImpl#getRequested_attributes <em>Requested attributes</em>}</li>
 *   <li>{@link carisma.profile.umlsec.umlsec4ids.impl.OwnerImpl#getRequested_actions <em>Requested actions</em>}</li>
 * </ul>
 *
 * @generated
 */
public class OwnerImpl extends MinimalEObjectImpl.Container implements Owner {
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
	 * The cached value of the '{@link #getProtected() <em>Protected</em>}' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getProtected()
	 * @generated
	 * @ordered
	 */
	protected EList<Action> protected_;

	/**
	 * The cached value of the '{@link #getRequested_attributes() <em>Requested attributes</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRequested_attributes()
	 * @generated
	 * @ordered
	 */
	protected EList<String> requested_attributes;

	/**
	 * The cached value of the '{@link #getRequested_actions() <em>Requested actions</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getRequested_actions()
	 * @generated
	 * @ordered
	 */
	protected EList<String> requested_actions;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected OwnerImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return Umlsec4idsPackage.Literals.OWNER;
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, Umlsec4idsPackage.OWNER__BASE_ACTIVITY_PARTITION, oldBase_ActivityPartition, base_ActivityPartition));
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
			eNotify(new ENotificationImpl(this, Notification.SET, Umlsec4idsPackage.OWNER__BASE_ACTIVITY_PARTITION, oldBase_ActivityPartition, base_ActivityPartition));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<Action> getProtected() {
		if (protected_ == null) {
			protected_ = new EObjectResolvingEList<Action>(Action.class, this, Umlsec4idsPackage.OWNER__PROTECTED);
		}
		return protected_;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getRequested_attributes() {
		if (requested_attributes == null) {
			requested_attributes = new EDataTypeUniqueEList<String>(String.class, this, Umlsec4idsPackage.OWNER__REQUESTED_ATTRIBUTES);
		}
		return requested_attributes;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<String> getRequested_actions() {
		if (requested_actions == null) {
			requested_actions = new EDataTypeUniqueEList<String>(String.class, this, Umlsec4idsPackage.OWNER__REQUESTED_ACTIONS);
		}
		return requested_actions;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case Umlsec4idsPackage.OWNER__BASE_ACTIVITY_PARTITION:
				if (resolve) return getBase_ActivityPartition();
				return basicGetBase_ActivityPartition();
			case Umlsec4idsPackage.OWNER__PROTECTED:
				return getProtected();
			case Umlsec4idsPackage.OWNER__REQUESTED_ATTRIBUTES:
				return getRequested_attributes();
			case Umlsec4idsPackage.OWNER__REQUESTED_ACTIONS:
				return getRequested_actions();
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
			case Umlsec4idsPackage.OWNER__BASE_ACTIVITY_PARTITION:
				setBase_ActivityPartition((ActivityPartition)newValue);
				return;
			case Umlsec4idsPackage.OWNER__PROTECTED:
				getProtected().clear();
				getProtected().addAll((Collection<? extends Action>)newValue);
				return;
			case Umlsec4idsPackage.OWNER__REQUESTED_ATTRIBUTES:
				getRequested_attributes().clear();
				getRequested_attributes().addAll((Collection<? extends String>)newValue);
				return;
			case Umlsec4idsPackage.OWNER__REQUESTED_ACTIONS:
				getRequested_actions().clear();
				getRequested_actions().addAll((Collection<? extends String>)newValue);
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
			case Umlsec4idsPackage.OWNER__BASE_ACTIVITY_PARTITION:
				setBase_ActivityPartition((ActivityPartition)null);
				return;
			case Umlsec4idsPackage.OWNER__PROTECTED:
				getProtected().clear();
				return;
			case Umlsec4idsPackage.OWNER__REQUESTED_ATTRIBUTES:
				getRequested_attributes().clear();
				return;
			case Umlsec4idsPackage.OWNER__REQUESTED_ACTIONS:
				getRequested_actions().clear();
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
			case Umlsec4idsPackage.OWNER__BASE_ACTIVITY_PARTITION:
				return base_ActivityPartition != null;
			case Umlsec4idsPackage.OWNER__PROTECTED:
				return protected_ != null && !protected_.isEmpty();
			case Umlsec4idsPackage.OWNER__REQUESTED_ATTRIBUTES:
				return requested_attributes != null && !requested_attributes.isEmpty();
			case Umlsec4idsPackage.OWNER__REQUESTED_ACTIONS:
				return requested_actions != null && !requested_actions.isEmpty();
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

		StringBuilder result = new StringBuilder(super.toString());
		result.append(" (requested_attributes: ");
		result.append(requested_attributes);
		result.append(", requested_actions: ");
		result.append(requested_actions);
		result.append(')');
		return result.toString();
	}

} //OwnerImpl
